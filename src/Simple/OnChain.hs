{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simple.OnChain where 

import PlutusTx
import PlutusTx.Prelude
import qualified Ledger
import qualified Plutus.V2.Ledger.Api           as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts
import Ledger (mkValidatorScript)


-- ----------------------------------------------------------------------
-- On-chain

{-# INLINEABLE simple #-}
simple :: BuiltinData -> BuiltinData -> BuiltinData -> ()
simple _ _ _ = ()

simpleCompile :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
simpleCompile = $$(compile [|| simple ||])

validator :: V2UtilsScripts.Validator
validator = Scripts.mkValidatorScript simpleCompile

simpleHash :: V2UtilsScripts.ValidatorHash
simpleHash = V2UtilsScripts.validatorHash validator

simpleAddress :: Ledger.Address
simpleAddress = Ledger.scriptHashAddress simpleHash
