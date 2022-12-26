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
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Prelude                                         as P
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1
import qualified Ledger.Ada                                      as Ada
import qualified Ledger (PaymentPubKeyHash, unPaymentPubKeyHash)
import qualified Plutus.V2.Ledger.Contexts as Context

-- ----------------------------------------------------------------------
-- Data types
-- ----------------------------------------------------------------------

data Dat = Dat
  { beneficiary1 :: Ledger.PaymentPubKeyHash
  , beneficiary2 :: Ledger.PaymentPubKeyHash
  , deadline     :: V2LedgerApi.POSIXTime
  } deriving P.Show

PlutusTx.unstableMakeIsData ''Dat

-- ----------------------------------------------------------------------
-- Set Datum and Redeemer Types
-- ----------------------------------------------------------------------

data VTypes
instance V2UtilsTypeScripts.ValidatorTypes VTypes where 
  type instance RedeemerType VTypes = ()
  type instance DatumType    VTypes = Dat

-- ----------------------------------------------------------------------
-- Validator Script
-- ----------------------------------------------------------------------

{-# INLINEABLE validateSigner #-}
validateSigner :: Dat -> () -> Contexts.ScriptContext -> Bool
validateSigner dat _ context =
    traceIfFalse "Wrong pubkeyhash or deadline problem" $
      (checkSigner (beneficiary1 dat) && beforeDeadline) ||
      (checkSigner (beneficiary2 dat) && afterDeadline)
  where
    txInfo :: Contexts.TxInfo
    txInfo = Contexts.scriptContextTxInfo context
    
    -- check if the tx was signed by either beneficiary
    checkSigner :: Ledger.PaymentPubKeyHash -> Bool
    checkSigner pkh = Contexts.txSignedBy txInfo
      (Ledger.unPaymentPubKeyHash pkh)

    -- check if tx interval is before or after the deadline
    beforeDeadline :: Bool
    beforeDeadline = LedgerIntervalV1.contains
      (LedgerIntervalV1.to $ deadline dat)
      (Context.txInfoValidRange txInfo)

    afterDeadline :: Bool
    afterDeadline = LedgerIntervalV1.contains
      (LedgerIntervalV1.from $ 1 + deadline dat)
      (Context.txInfoValidRange txInfo)
    
-- ----------------------------------------------------------------------
-- Boilerplate 
-- ----------------------------------------------------------------------

typedValidator :: V2UtilsTypeScripts.TypedValidator VTypes
typedValidator = V2UtilsTypeScripts.mkTypedValidator @VTypes
    $$(compile [|| validateSigner ||])
    $$(compile [|| wrap ||])
  where
    wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @()

validator :: V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript typedValidator

validatorHash :: V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash typedValidator

address :: V1LAddress.Address
address = V1LAddress.scriptHashAddress validatorHash
