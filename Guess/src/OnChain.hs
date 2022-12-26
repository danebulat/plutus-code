{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module OnChain where

import PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Address                                  as V1LAddress
import qualified Plutus.V2.Ledger.Api                            as V2LedgerApi
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Prelude                                         as P
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1
import qualified Ledger.Ada                                      as Ada
import qualified Ledger (PaymentPubKeyHash, unPaymentPubKeyHash)

-- ----------------------------------------------------------------------
-- Data types
-- ----------------------------------------------------------------------

newtype Redeem = Redeem
  { redeem :: Integer
  } deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem

newtype Dat = Dat
  { dData :: Integer
  } deriving P.Show

PlutusTx.unstableMakeIsData ''Dat

-- ----------------------------------------------------------------------
-- Validator types
-- ----------------------------------------------------------------------

data VTypes
instance V2UtilsTypeScripts.ValidatorTypes VTypes where
  type instance RedeemerType VTypes = Redeem
  type instance DatumType    VTypes = Dat

-- ----------------------------------------------------------------------
-- Validator script
-- ----------------------------------------------------------------------

{-# INLINEABLE simpleValidator #-}
simpleValidator :: Dat -> Redeem -> Contexts.ScriptContext -> Bool
simpleValidator d r _ = traceIfFalse "Incorrect guess" (dData d == redeem r)

-- ----------------------------------------------------------------------
-- Boilerplate
-- ----------------------------------------------------------------------

simpleTypeV :: V2UtilsTypeScripts.TypedValidator VTypes
simpleTypeV = V2UtilsTypeScripts.mkTypedValidator @VTypes
    $$(PlutusTx.compile [|| simpleValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

validator :: V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript simpleTypeV

validatorHash :: V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash simpleTypeV

address :: V1LAddress.Address
address = V1LAddress.scriptHashAddress validatorHash
