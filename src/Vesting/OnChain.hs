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

module Vesting.OnChain where

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
import qualified Plutus.V2.Ledger.Contexts as Context


-- ----------------------------------------------------------------------
-- Data types 

data Dat = Dat
  { beneficiary :: Ledger.PaymentPubKeyHash
  , deadline    :: V2LedgerApi.POSIXTime
  } deriving P.Show

PlutusTx.unstableMakeIsData ''Dat


-- ----------------------------------------------------------------------
-- Set Datum and Redeemer types 

data VTypes
instance V2UtilsTypeScripts.ValidatorTypes VTypes where
  type instance RedeemerType VTypes = ()
  type instance DatumType    VTypes = Dat


-- ----------------------------------------------------------------------
-- Validator script 

{-# INLINEABLE mkValidator #-}
mkValidator :: Dat -> () -> Context.ScriptContext -> Bool
mkValidator dat _ context = signedByBeneficiary && deadlinePassed
  where
    txInfo :: Contexts.TxInfo
    txInfo = Contexts.scriptContextTxInfo context

    signedByBeneficiary :: Bool
    signedByBeneficiary = Context.txSignedBy txInfo
      (Ledger.unPaymentPubKeyHash $ beneficiary dat)

    deadlinePassed :: Bool
    deadlinePassed = LedgerIntervalV1.contains
      (LedgerIntervalV1.from $ deadline dat)
      (V2LedgerApi.txInfoValidRange txInfo)


-- ----------------------------------------------------------------------
-- Boilerplate 

typedValidator :: V2UtilsTypeScripts.TypedValidator VTypes
typedValidator = V2UtilsTypeScripts.mkTypedValidator @VTypes
    $$(compile [|| mkValidator ||])
    $$(compile [|| wrap ||])
  where
    wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @()

validator :: V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript typedValidator

validatorHash :: V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash typedValidator

address :: V1LAddress.Address
address = V1LAddress.scriptHashAddress validatorHash
