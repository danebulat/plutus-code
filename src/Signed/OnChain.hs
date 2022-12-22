-- Extensions
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Required to use custom data types
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Signed.OnChain where

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
import qualified Ledger.Ada                                      as Ada
import qualified Ledger
import qualified
  Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV2

{-
Demonstrates a parameterized minting policy.
The minting policy script takes a public key hash as a parameter.
Minting and burning is only allowed if the owner of that pkh
has signed the minting or burning transaction.
-}

-- ----------------------------------------------------------------------
-- On-chain

{-# INLINEABLE mkPolicy #-}
mkPolicy :: Ledger.PaymentPubKeyHash -> () -> Contexts.ScriptContext -> Bool
mkPolicy pkh () ctx = Contexts.txSignedBy
    (Contexts.scriptContextTxInfo ctx)
    (Ledger.unPaymentPubKeyHash pkh)

-- ----------------------------------------------------------------------
-- Boilerplate 

policy :: Ledger.PaymentPubKeyHash -> V2LedgerApi.MintingPolicy
policy pkh = V2LedgerApi.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.mkUntypedMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

curSymbol :: Ledger.PaymentPubKeyHash -> Ledger.CurrencySymbol
curSymbol = UtilsScriptsV2.scriptCurrencySymbol . policy

policyScript :: Ledger.PaymentPubKeyHash -> V2LedgerApi.Script
policyScript = V2LedgerApi.unMintingPolicyScript . policy

mintValidator :: Ledger.PaymentPubKeyHash -> V2LedgerApi.Validator
mintValidator = V2LedgerApi.Validator . policyScript
