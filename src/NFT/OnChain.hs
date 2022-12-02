-- Extensions
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-- Required to use custom data types
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module NFT.OnChain where

import PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Address                                  as V1LAddress
import qualified Plutus.V2.Ledger.Api                            as V2LedgerApi
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.Script.Utils.V2.Scripts                  as UtilsScriptsV2
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts            as Scripts
import qualified Prelude                                         as P
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1
import qualified Plutus.V1.Ledger.Value                          as Value
import qualified Ledger.Ada                                      as Ada
import qualified Ledger
--import qualified
--  Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV2

-- ----------------------------------------------------------------------
-- On-chain

-- Two parameters: the utxo output and token name of NFT
-- Could also use a data type and one parameter.

{-# INLINEABLE mkPolicy #-}
mkPolicy
    :: V2LedgerApi.TxOutRef
    -> V2LedgerApi.TokenName
    -> ()
    -> Contexts.ScriptContext
    -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumd" hasUtxo &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    txInfo :: V2LedgerApi.TxInfo
    txInfo = Contexts.scriptContextTxInfo ctx

    -- Check if UTXO param has been provided as a TX input
    hasUtxo :: Bool
    hasUtxo = any (\i -> V2LedgerApi.txInInfoOutRef i == oref) $
                V2LedgerApi.txInfoInputs txInfo

    -- Check mint amount is exactly 1 and token name matches
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (V2LedgerApi.txInfoMint txInfo) of
      [(_, tn', amt)] -> tn' == tn && amt == 1
      _               -> False


-- ----------------------------------------------------------------------
-- Boilerplate 

policy :: V2LedgerApi.TxOutRef -> V2LedgerApi.TokenName -> V2LedgerApi.MintingPolicy
policy oref tn = V2LedgerApi.mkMintingPolicyScript $
    $$(PlutusTx.compile
       [|| \oref' tn' -> Scripts.mkUntypedMintingPolicy (mkPolicy oref' tn') ||]
      )
    `PlutusTx.applyCode` PlutusTx.liftCode oref
    `PlutusTx.applyCode` PlutusTx.liftCode tn

curSymbol :: V2LedgerApi.TxOutRef -> V2LedgerApi.TokenName -> Ledger.CurrencySymbol
curSymbol oref tn = UtilsScriptsV2.scriptCurrencySymbol $ policy oref tn

policyScript :: V2LedgerApi.TxOutRef -> V2LedgerApi.TokenName -> V2LedgerApi.Script
policyScript oref tn = V2LedgerApi.unMintingPolicyScript $ policy oref tn

mintValidator :: V2LedgerApi.TxOutRef -> V2LedgerApi.TokenName -> V2LedgerApi.Validator
mintValidator oref tn = V2LedgerApi.Validator $ policyScript oref tn
