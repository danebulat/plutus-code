{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}

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

-- -------------------------------------------------------------------------------
-- Data types
-- -------------------------------------------------------------------------------

newtype Redeem = Redeem
  { redeem :: Integer
  } deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem

data Dat = Dat
  { creator     :: Ledger.PaymentPubKeyHash
  , beneficiary :: Ledger.PaymentPubKeyHash
  , deadline    :: V2LedgerApi.POSIXTime
  , dData       :: Integer
  } deriving P.Show

PlutusTx.unstableMakeIsData ''Dat

-- --------------------------------------------------------------------------------
-- Set Datum and Redeemer Types
-- -------------------------------------------------------------------------------

data Simple
instance V2UtilsTypeScripts.ValidatorTypes Simple where
    type instance RedeemerType Simple = Redeem
    type instance DatumType    Simple = Dat

-- --------------------------------------------------------------------------------
-- Validator Script 
-- -------------------------------------------------------------------------------

{-# INLINEABLE simpleType #-}
simpleType :: Dat -> Redeem -> Contexts.ScriptContext -> Bool
simpleType d r context =
       traceIfFalse "Sorry the guess is not correct" (dData d == redeem r)
    && traceIfFalse "Wrong pubkeyhash" signedBeneficiary
    && traceIfFalse "Deadline not yet reached" deadlinePassed
    && traceIfFalse "Not paid royalties" calculateRoyalties
  where
    txInfo :: Contexts.TxInfo
    txInfo = Contexts.scriptContextTxInfo context

    signedBeneficiary :: Bool
    signedBeneficiary = Contexts.txSignedBy txInfo
      (Ledger.unPaymentPubKeyHash $ beneficiary d)

    deadlinePassed :: Bool
    deadlinePassed = LedgerIntervalV1.contains
      (LedgerIntervalV1.from $ deadline d)
      (Contexts.txInfoValidRange txInfo)

    calculateRoyalties :: Bool
    calculateRoyalties = validateRoyalties d txInfo

-- --------------------------------------------------------------------------------
-- Validator script functions
-- -------------------------------------------------------------------------------

-- Check if creator is receiving minimum 10% of the tx value
{-# INLINEABLE validateRoyalties #-}
validateRoyalties :: Dat -> Contexts.TxInfo -> Bool
validateRoyalties d txInfo = compareValues (qCreator d txInfo) (totalValue txInfo)

-- Get total amount ADA from the transaction
{-# INLINEABLE totalValue #-}
totalValue :: Contexts.TxInfo -> Ada.Ada
totalValue txInfo = Ada.fromValue $ Contexts.valueSpent txInfo

-- Get value paid to the creator of the contract (10%)
{-# INLINEABLE qCreator #-}
qCreator :: Dat -> Contexts.TxInfo -> Ada.Ada
qCreator d txInfo = Ada.fromValue
    -- Get creator's pkh and check how much ADA is sent to him/her in the tx
    $ Contexts.valuePaidTo txInfo (Ledger.unPaymentPubKeyHash (creator d))

-- Return True if creator is receiving >= 10% of the total tx value
{-# INLINEABLE compareValues #-}
compareValues :: Ada.Ada -> Ada.Ada -> Bool
compareValues a a' = a' >= a `Ada.divide` 10

-- --------------------------------------------------------------------------------
-- Boilerplate
-- -------------------------------------------------------------------------------

simpleTypeV :: V2UtilsTypeScripts.TypedValidator Simple
simpleTypeV = V2UtilsTypeScripts.mkTypedValidator @Simple
    $$(compile [|| simpleType ||])
    $$(compile [|| wrap ||])
  where
    wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

validator :: V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript simpleTypeV

validatorHash :: V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash simpleTypeV

address :: V1LAddress.Address
address = V1LAddress.scriptHashAddress validatorHash
