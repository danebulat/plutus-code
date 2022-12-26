{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module OnChain where

import PlutusTx
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

-- ----------------------------------------------------------------------
-- On-chain 
-- ----------------------------------------------------------------------

{-# INLINEABLE mkTokenPolicy #-}
mkTokenPolicy
    :: LedgerApiV2.TxOutRef     -- utxo reference 
    -> LedgerApiV2.TokenName    -- token name to mint
    -> Integer                  -- amount of token to mint
    -> ()                       -- (redeemer) unit
    -> Contexts.ScriptContext   -- script context
    -> Bool
mkTokenPolicy oref tn amt () ctx =
    traceIfFalse "expected UTXO not in tx" hasUtxo &&
    traceIfFalse "invalid token name or amount" checkMintedAmount
  where
    info :: LedgerApiV2.TxInfo
    info = LedgerApiV2.scriptContextTxInfo ctx

    hasUtxo :: Bool
    hasUtxo = any (\i -> LedgerApiV2.txInInfoOutRef i == oref) $
                LedgerApiV2.txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue $ LedgerApiV2.txInfoMint info of
      [(_, tn', amt')] -> tn' == tn && amt' == amt
      _ -> False

-- ----------------------------------------------------------------------
-- Boilerplate 
-- ----------------------------------------------------------------------

-- minting policy script
policy
    :: LedgerApiV2.TxOutRef
    -> LedgerApiV2.TokenName
    -> Integer
    -> LedgerApiV2.MintingPolicy
policy oref tn n = LedgerApiV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' n' ->
         Scripts.mkUntypedMintingPolicy $ mkTokenPolicy oref' tn' n' ||]
      )
    `PlutusTx.applyCode` PlutusTx.liftCode oref
    `PlutusTx.applyCode` PlutusTx.liftCode tn
    `PlutusTx.applyCode` PlutusTx.liftCode n


-- hash of the minting policy Script
curSymbol
    :: LedgerApiV2.TxOutRef
    -> LedgerApiV2.TokenName
    -> Integer
    -> LedgerApiV2.CurrencySymbol
curSymbol oref tn n = UtilsScriptsV2.scriptCurrencySymbol $ policy oref tn n


-- extract Script from MintingPolicy
policyScript
    :: LedgerApiV2.TxOutRef
    -> LedgerApiV2.TokenName
    -> Integer
    -> LedgerApiV2.Script
policyScript oref tn n = LedgerApiV2.unMintingPolicyScript $ policy oref tn n


-- wrap Script in Validator instead of MintingPolicy
mintValidator
    :: LedgerApiV2.TxOutRef
    -> LedgerApiV2.TokenName
    -> Integer
    -> LedgerApiV2.Validator
mintValidator oref tn n = LedgerApiV2.Validator $ policyScript oref tn n
