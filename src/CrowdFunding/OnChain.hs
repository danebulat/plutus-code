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

import PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Address                                  as V1LAddress
import qualified Plutus.V2.Ledger.Api                            as V2LedgerApi
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Prelude                                         as P
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1
import qualified Ledger.Ada                                      as Ada
import qualified Ledger (PaymentPubKeyHash(..), unPaymentPubKeyHash)
import Data.Default


-- ----------------------------------------------------------------------
-- Data where

type Lovelace = Integer

-- Param
data CrowdParams = CrowdParams
  { beneficiary    :: Ledger.PaymentPubKeyHash
  , targetAmount   :: Ada.Ada
  , deadline       :: V2LedgerApi.POSIXTime
  }

instance Default CrowdParams where
  def = CrowdParams
          { beneficiary  = Ledger.PaymentPubKeyHash
                           "1ca9410b9c346768a410f8aa3599ad6ff134864e7381e2cb8c83db0a"
          , targetAmount = 100_000_000
          , deadline     = 1669063800000
          }

PlutusTx.unstableMakeIsData ''CrowdParams
PlutusTx.makeLift ''CrowdParams

-- Datum
newtype Dat = Dat
  { contributor :: Ledger.PaymentPubKeyHash }

PlutusTx.unstableMakeIsData ''Dat

-- Redeemer
-- TODO: Figure out how to calculate if the target was met on-chain.
-- Instead of using a Bool provided off-chain.
data Redeem =
  Contribute | Withdraw | Close Bool

PlutusTx.unstableMakeIsData ''Redeem


-- ----------------------------------------------------------------------
-- Set Datum and Redeemer types 

data VTypes
instance V2UtilsTypeScripts.ValidatorTypes VTypes where
  type instance RedeemerType VTypes = Redeem
  type instance DatumType    VTypes = Dat


-- ----------------------------------------------------------------------
-- Validator script 

{-# INLINEABLE validatorCrowdFundTx #-}
validatorCrowdFundTx
    :: CrowdParams
    -> Dat
    -> Redeem
    -> Contexts.ScriptContext
    -> Bool 
validatorCrowdFundTx params dat red context =
    case red of
      Contribute ->
        traceIfFalse "deadline passed" beforeDeadline
      
      Withdraw ->
        traceIfFalse "signer is not the contributor"
          (checkTxSignerWith $ contributor dat) &&
        traceIfFalse "deadline passed" beforeDeadline
      
      Close targetMet ->
        traceIfFalse "signer is not the beneficiary"
                     (checkTxSignerWith $ beneficiary params)   &&
        traceIfFalse "deadline not passed" (not beforeDeadline) &&
        traceIfFalse "target not met" targetMet

        -- TODO: Figure out how to refund contributors if
        -- the target was not met.
  where
    txInfo :: V2LedgerApi.TxInfo
    txInfo = V2LedgerApi.scriptContextTxInfo context

    beforeDeadline :: Bool
    beforeDeadline = LedgerIntervalV1.contains
      (LedgerIntervalV1.to $ deadline params)
      (Contexts.txInfoValidRange txInfo)

    checkTxSignerWith :: Ledger.PaymentPubKeyHash -> Bool
    checkTxSignerWith pkh = Contexts.txSignedBy txInfo contributorPkh
      where
        contributorPkh = Ledger.unPaymentPubKeyHash pkh


-- ----------------------------------------------------------------------
-- Boilerplate 

validatorCrowdFundTxTyped
    :: CrowdParams
    -> V2UtilsTypeScripts.TypedValidator VTypes
validatorCrowdFundTxTyped crowdParams =
  V2UtilsTypeScripts.mkTypedValidator @VTypes
    ($$( PlutusTx.compile [|| validatorCrowdFundTx ||])
        `PlutusTx.applyCode`
         PlutusTx.liftCode crowdParams)
    $$( PlutusTx.compile [|| wrap ||] )
  where
    wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

validator :: CrowdParams -> V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript . validatorCrowdFundTxTyped

validatorHash :: CrowdParams -> V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash . validatorCrowdFundTxTyped

address :: CrowdParams -> V1LAddress.Address
address = V1LAddress.scriptHashAddress . validatorHash

