{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Week08.OnChain where

import qualified GHC.Generics                                    as GHCGenerics (Generic)
import qualified Data.Aeson                                      as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                             as DataOpenApiSchema (ToSchema)

import PlutusTx
import qualified PlutusTx.Coverage                               as PtxCoverage
import qualified PlutusTx.Code                                   as PtxCode
import PlutusTx.Prelude
import qualified Ledger.Address                                  as LAddressV1
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.Script.Utils.V2.Scripts                  as UtilsScriptsV2
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as UtilsTypeScriptsV2
import qualified Plutus.Script.Utils.V2.Typed.Scripts            as Scripts
import qualified Prelude                                         as P
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1
import qualified Plutus.V1.Ledger.Value                          as Value
import qualified Ledger.Ada                                      as Ada
import qualified Ledger
import qualified Wallet.Emulator.Wallet as Ada
import qualified Ledger as LAddress.V1


-- ----------------------------------------------------------------------
-- Contract Parameter

data TokenSale = TokenSale
  { tsSeller :: Ledger.PaymentPubKeyHash
  , tsToken  :: Ledger.AssetClass
  , tsTT     :: Ledger.AssetClass
  } deriving (P.Show,
              P.Eq,
              GHCGenerics.Generic,
              DataAeson.FromJSON,
              DataAeson.ToJSON)

PlutusTx.unstableMakeIsData ''TokenSale
PlutusTx.makeLift ''TokenSale

-- ----------------------------------------------------------------------
-- Redeemer

data TSRedeemer =
       SetPrice  Integer          -- update token price in datum 
     | AddTokens Integer          -- add tokens to contract UTXO
     | BuyTokens Integer          -- swap tokens with ADA
     | Withdraw  Integer Integer  -- withdaw tokens and lovelace
     deriving (P.Show, P.Eq)
   
PlutusTx.unstableMakeIsData ''TSRedeemer

-- ----------------------------------------------------------------------
-- Helper Functions

{-# INLINABLE lovelaces #-}
lovelaces :: Value.Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE getUtxoDatum #-}
getUtxoDatum :: Contexts.TxOut -> Integer
getUtxoDatum out = case LedgerApiV2.txOutDatum out of
  LedgerApiV2.NoOutputDatum     -> traceError "no datum in own output"
  LedgerApiV2.OutputDatumHash _ -> traceError "datum hash not expected"
  LedgerApiV2.OutputDatum d     ->
    case PlutusTx.fromBuiltinData $ LedgerApiV2.getDatum d of
      Nothing -> traceError "error converting Datum to Integer"
      Just dat -> dat

-- ----------------------------------------------------------------------
-- Validator

{-# INLINABLE mkTokenSaleValidator #-}
mkTokenSaleValidator
    :: TokenSale
    -> Integer
    -> TSRedeemer
    -> Contexts.ScriptContext
    -> Bool
mkTokenSaleValidator ts dat red context =

    -- Check spending UTXO has the thread token
    traceIfFalse "token missing from input" inputHasThreadToken &&

    case (ownInputValue, dat, red) of

      -- Set price operation
      (v, _, SetPrice p) | p >= 0 ->
        traceIfFalse "not signed by seller"      signedBySeller       &&
        traceIfFalse "wrong output datum"        (outputDatum == p)   &&
        traceIfFalse "token missing from output" outputHasThreadToken &&
        traceIfFalse "wrong output value"        (ownOutputValue == v)
        
      -- Add tokens operation
      (v, p, AddTokens n) | n > 0 ->
        traceIfFalse "wrong output datum"        (outputDatum == p)   &&
        traceIfFalse "token missing from output" outputHasThreadToken &&
        traceIfFalse "wrong output value"
          (ownOutputValue == v
                        P.<> Value.assetClassValue (tsToken ts) n)

      -- Buy tokens operation
      (v, p, BuyTokens n) | n > 0 ->
        traceIfFalse "wrong output datum"        (outputDatum == p)   &&
        traceIfFalse "token missing from output" outputHasThreadToken &&
        traceIfFalse "wrong output value"
          (ownOutputValue == v
                        P.<> Value.assetClassValue (tsToken ts) (negate n)
                        P.<> Ada.lovelaceValueOf (n * p))

      -- Withdraw tokens operation
      (v, p, Withdraw n l)
        | n >= 0 &&
          l >= 0 &&
          v `Value.geq` (withdrawAsValue n l <> Ada.toValue Ledger.minAdaTxOut) ->
        traceIfFalse "not signed by seller"      signedBySeller       &&
        traceIfFalse "wrong output datum"        (outputDatum == p)   &&
        traceIfFalse "token missing from output" outputHasThreadToken &&
        traceIfFalse "wrong output value"
          (ownOutputValue == v
                        P.<> Value.assetClassValue (tsToken ts) (negate n)
                        P.<> Ada.lovelaceValueOf (negate l))

      -- Invalid transaction
      _ -> False

  where
    txInfo :: LedgerApiV2.TxInfo
    txInfo = Contexts.scriptContextTxInfo context
    
    ownInput :: LedgerApiV2.TxOut
    ownInput = case Contexts.findOwnInput context of
        Nothing       -> traceError "token sale input missing"
        Just txInInfo -> LedgerApiV2.txInInfoResolved txInInfo

    ownInputValue :: Value.Value
    ownInputValue = LedgerApiV2.txOutValue ownInput

    withdrawAsValue :: Integer -> Integer -> Value.Value
    withdrawAsValue n l = Value.assetClassValue (tsToken ts) n <>
                          Ada.lovelaceValueOf l

    signedBySeller :: Bool
    signedBySeller =
      Contexts.txSignedBy txInfo (Ledger.unPaymentPubKeyHash $ tsSeller ts)

    -- Check own input has thread token
    inputHasThreadToken :: Bool
    inputHasThreadToken = n == 1
      where n = Value.assetClassValueOf
                  (LedgerApiV2.txOutValue ownInput) (tsTT ts)

    -- Gets all outputs that pay to the script address we're spending from
    ownOutput :: LedgerApiV2.TxOut
    ownOutput = case Contexts.getContinuingOutputs context of
      [o] -> o
      _   -> traceError "expected exactly one output to script address"

    -- Check own output contains the thread token
    outputHasThreadToken :: Bool
    outputHasThreadToken = n == 1
      where n = Value.assetClassValueOf
                  (LedgerApiV2.txOutValue ownOutput) (tsTT ts)

    -- Get own output datum
    outputDatum :: Integer
    outputDatum = getUtxoDatum ownOutput

    -- Get own output Value
    ownOutputValue :: Value.Value
    ownOutputValue = LedgerApiV2.txOutValue ownOutput

-- ----------------------------------------------------------------------
-- Boilerplate

data TS
instance UtilsTypeScriptsV2.ValidatorTypes TS where
  type instance DatumType    TS = Integer
  type instance RedeemerType TS = TSRedeemer

tsTypedValidator :: TokenSale -> UtilsTypeScriptsV2.TypedValidator TS
tsTypedValidator ts = UtilsTypeScriptsV2.mkTypedValidator @TS
    ($$(PlutusTx.compile [|| mkTokenSaleValidator ||])
       `PlutusTx.applyCode` PlutusTx.liftCode ts)
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = UtilsTypeScriptsV2.mkUntypedValidator @Integer @TSRedeemer

tsValidator :: TokenSale -> LedgerApiV2.Validator
tsValidator = UtilsTypeScriptsV2.validatorScript . tsTypedValidator

tsValidatorHash :: TokenSale -> LedgerApiV2.ValidatorHash
tsValidatorHash = UtilsTypeScriptsV2.validatorHash . tsTypedValidator

tsScriptAddress :: TokenSale -> LAddressV1.Address
tsScriptAddress = LAddress.V1.scriptHashAddress . tsValidatorHash

-- ----------------------------------------------------------------------
-- Coverage Index

tsCovIdx :: PtxCoverage.CoverageIndex
tsCovIdx = PtxCode.getCovIdx  $$(PlutusTx.compile [|| mkTokenSaleValidator ||])

