-- Extensions
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}

-- Required to use custom data types
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CrowdFunding.OnChain where

import Data.Default
import qualified Data.Map                                        as Map

import PlutusTx
import PlutusTx.Prelude
import qualified Ledger                                          as L
import qualified Ledger.Address                                  as LAddressV1
import qualified Plutus.V2.Ledger.Api                            as LV2
import qualified Plutus.V2.Ledger.Contexts                       as Ctx
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Prelude                                         as P
import qualified Plutus.V1.Ledger.Interval                       as I
import qualified Plutus.V1.Ledger.Value                          as V
import qualified Ledger.Ada                                      as Ada
import Ledger.Value (assetClassValueOf)

-- ----------------------------------------------------------------------
-- Parameter

type Lovelace = Integer

data Param = Param
  { pTT          :: !L.AssetClass
  , pBeneficiary :: !L.PaymentPubKeyHash
  , pCreator     :: !L.PaymentPubKeyHash
  , pDeadline    :: !L.POSIXTime
  , pTarget      :: !Lovelace
  } deriving (P.Show, P.Eq)

PlutusTx.unstableMakeIsData ''Param
PlutusTx.makeLift ''Param

-- ----------------------------------------------------------------------
-- Datum

newtype Dat = Dat
  { dContributors :: [(L.PaymentPubKeyHash, V.Value)]
  }

PlutusTx.unstableMakeIsData ''Dat
PlutusTx.makeLift ''Dat

-- ----------------------------------------------------------------------
-- Redeemer

data Redeem
  = AddTokens    !Lovelace  -- contributor adds lovelace
  | RemoveTokens !Lovelace  -- contributor removes lovelace
  | Withdraw                -- beneficiary withdraws lovelace

PlutusTx.unstableMakeIsData ''Redeem

-- ----------------------------------------------------------------------
-- Set Datum and Redeemer types 

data VTypes
instance V2UtilsTypeScripts.ValidatorTypes VTypes where
  type instance RedeemerType VTypes = Redeem
  type instance DatumType    VTypes = Dat

-- ----------------------------------------------------------------------
-- Validator script 

{-# INLINABLE getUtxoDatum #-}
getUtxoDatum :: Ctx.TxOut -> Dat
getUtxoDatum out = case LV2.txOutDatum out of
  LV2.NoOutputDatum     -> traceError "no datum in own output"
  LV2.OutputDatumHash _ -> traceError "datum hash not expected"
  LV2.OutputDatum d     ->
    case PlutusTx.fromBuiltinData $ LV2.getDatum d of
      Nothing -> traceError "error converting Datum to Integer"
      Just dat -> dat

{-# INLINEABLE validateCrowdfunding #-}
validateCrowdfunding :: Param -> Dat -> Redeem -> Ctx.ScriptContext -> Bool 
validateCrowdfunding param dat red ctx =

  -- Check spending UTXO has the thread token
  traceIfFalse "token missing from input" inputHasThreadToken &&

  case red of

    -- Add lovelace to contract
    AddTokens n ->
      traceIfFalse "deadline passed"                beforeDeadline       &&
      traceIfFalse "thread token missing in output" outputHasThreadToken &&
      traceIfFalse "wrong output datum"             False                &&
      traceIfFalse "wrong output value"             False
                                                 
    RemoveTokens n ->
      traceIfFalse "deadline passed"                beforeDeadline       &&
      traceIfFalse "thread token missing in output" outputHasThreadToken &&
      traceIfFalse "wrong output datum"             False                &&
      traceIfFalse "wrong output value"             False

    Withdraw ->
      traceIfFalse "deadline not passed"            (not beforeDeadline) &&
      traceIfFalse "thread token missing in output" outputHasThreadToken &&
      traceIfFalse "target not met"                 targetMet            &&
      traceIfFalse "not signed by beneficiary"      (checkTxSigner $ pBeneficiary param) &&
      traceIfFalse "royalty not paid to creator"    False

    -- Invalid transaction
    _ -> False

  where
    txInfo :: LV2.TxInfo
    txInfo = LV2.scriptContextTxInfo ctx

    beforeDeadline :: Bool
    beforeDeadline = I.contains (I.to $ pDeadline param) (Ctx.txInfoValidRange txInfo)

    checkTxSigner :: L.PaymentPubKeyHash -> Bool
    checkTxSigner pkh = Ctx.txSignedBy txInfo (L.unPaymentPubKeyHash pkh)

    -- Target met
    targetMet :: Bool
    targetMet = lovelace >= pTarget param
      where lovelace = Ada.getLovelace $ Ada.fromValue (LV2.txOutValue ownInput)

    -- --------------------------------------------------
    -- Own input checking
    
    -- Get input currently being validated
    ownInput :: LV2.TxOut
    ownInput = case Ctx.findOwnInput ctx of
      Nothing -> traceError "own script input missing"
      Just txInInfo -> LV2.txInInfoResolved txInInfo

    inputHasThreadToken :: Bool
    inputHasThreadToken = n == 1
      where n = V.assetClassValueOf (LV2.txOutValue ownInput) (pTT param)

    -- --------------------------------------------------
    -- Own output checking

    -- Get all outputs that pay to the script address we're spending from
    ownOutput :: LV2.TxOut
    ownOutput = case Ctx.getContinuingOutputs ctx of
      [o] -> o
      _   -> traceError "expected exactly one output to script address"

    outputHasThreadToken :: Bool
    outputHasThreadToken = n == 1
      where n = V.assetClassValueOf  (LV2.txOutValue ownOutput) (pTT param)

    -- --------------------------------------------------
    -- Datum checking

    -- Make sure tx is signed by one pkh
    getSignerPkh :: L.PaymentPubKeyHash
    getSignerPkh = case LV2.txInfoSignatories txInfo of
      [pkh] -> L.PaymentPubKeyHash pkh
      _     -> traceError "more than one signer"
    
    -- Get own output datum
    outputDatum :: Dat
    outputDatum = getUtxoDatum ownOutput
    
    -- Check datum
    checkDatum :: Bool
    checkDatum = True


-- ----------------------------------------------------------------------
-- Boilerplate 

validatorCrowdfundTyped :: Param -> V2UtilsTypeScripts.TypedValidator VTypes
validatorCrowdfundTyped param =
  V2UtilsTypeScripts.mkTypedValidator @VTypes
    ($$( PlutusTx.compile [|| validateCrowdfunding ||])
        `PlutusTx.applyCode`
         PlutusTx.liftCode param)
    $$( PlutusTx.compile [|| wrap ||] )
  where
    wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

validator :: Param -> L.Validator
validator = V2UtilsTypeScripts.validatorScript . validatorCrowdfundTyped

validatorHash :: Param -> L.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash . validatorCrowdfundTyped

address :: Param -> L.Address
address = LAddressV1.scriptHashAddress . validatorHash

