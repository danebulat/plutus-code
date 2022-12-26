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

-- ----------------------------------------------------------------------
-- On-chain
-- ----------------------------------------------------------------------

{-# INLINEABLE mkPolicy #-}
mkPolicy :: () -> Contexts.ScriptContext -> Bool
mkPolicy () _ = True

-- ----------------------------------------------------------------------
-- Boilerplate
-- ----------------------------------------------------------------------

policy :: Scripts.MintingPolicy
policy = V2LedgerApi.mkMintingPolicyScript
  $$(PlutusTx.compile [|| Scripts.mkUntypedMintingPolicy mkPolicy ||])

-- CurrencySymbol (plutus-ledger-api)
curSymbol :: Ledger.CurrencySymbol
curSymbol = UtilsScriptsV2.scriptCurrencySymbol policy

-- Script (plutus-ledger-api)
policyScript :: V2LedgerApi.Script
policyScript = V2LedgerApi.unMintingPolicyScript policy

-- Validator (plutus-ledger-api)
mintValidator :: V2LedgerApi.Validator
mintValidator = V2LedgerApi.Validator policyScript

-- Convert a `Builtins.BuiltinsData` value to a `cardano-api` script value 
scriptHash :: V2LedgerApi.ScriptHash 
scriptHash = UtilsScriptsV2.scriptHash policyScript

-- Convert a `Builtins.BuiltinsData` value to a `cardano-api` script value 
validatorHash :: UtilsScriptsV2.ValidatorHash
validatorHash = UtilsScriptsV2.validatorHash mintValidator

-- Script runting representation of a `Digest SHA256`
mintingPolicyHash :: UtilsScriptsV2.MintingPolicyHash
mintingPolicyHash = UtilsScriptsV2.mintingPolicyHash policy

