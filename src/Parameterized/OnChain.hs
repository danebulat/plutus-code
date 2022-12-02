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

module Parameterized.OnChain where

import PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Address                                  as V1LAddress
import qualified Plutus.V2.Ledger.Api                            as V2LedgerApi
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Prelude                                         as P
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1
import qualified Ledger.Ada                                      as Ada
import qualified Ledger

-- -------------------------------------------------------------------------------
-- Data types

data BenParam = BenParam
  { creator     :: Ledger.PaymentPubKeyHash
  , beneficiary :: Ledger.PaymentPubKeyHash
  , deadline    :: V2LedgerApi.POSIXTime
  } deriving P.Show

PlutusTx.unstableMakeIsData ''BenParam
PlutusTx.makeLift ''BenParam

newtype Redeem = Redeem
  { redeem :: Integer
  } deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem

newtype Dat = Dat
  { dData :: Integer
  } deriving P.Show

PlutusTx.unstableMakeIsData ''Dat


-- --------------------------------------------------------------------------------
-- Set Datum and Redeemer Types

data Simple
instance V2UtilsTypeScripts.ValidatorTypes Simple where
    type instance RedeemerType Simple = Redeem
    type instance DatumType    Simple = Dat


-- --------------------------------------------------------------------------------
-- Validator Script 

{-# INLINEABLE simpleType #-}
simpleType :: BenParam -> Dat -> Redeem -> Contexts.ScriptContext -> Bool
simpleType benp d r context =
       traceIfFalse "Guess is not correct"     (dData d == redeem r)
    && traceIfFalse "Wrong pubkeyhash"         signedBeneficiary
    && traceIfFalse "Deadline not yet reached" deadlinePassed
    && traceIfFalse "Not paid royalties"       calculateRoyalties
  where
    txInfo :: Contexts.TxInfo
    txInfo = Contexts.scriptContextTxInfo context

    signedBeneficiary :: Bool
    signedBeneficiary = Contexts.txSignedBy txInfo
      (Ledger.unPaymentPubKeyHash $ beneficiary benp)

    deadlinePassed :: Bool
    deadlinePassed = LedgerIntervalV1.contains
      (LedgerIntervalV1.from $ deadline benp)
      (Contexts.txInfoValidRange txInfo)

    -- Calculate minimum royalites to pay to creator
    -- NOTE: If more than one input exists, add all input values to calculate the 10%
    adaRoyalties :: Maybe Ada.Ada
    adaRoyalties = do
      validatedValue <- Contexts.txOutValue .    -- get value of tx that will be spent
                  Contexts.txInInfoResolved <$>  -- get output to spend (TxOut)
                  Contexts.findOwnInput context  -- get script input being validated (Maybe TxInInfo)
      Just $ Ada.fromValue validatedValue `Ada.divide` 10

    -- Get value paid to creator's pkh in the transaction
    getValuePaidToCreator :: Ada.Ada
    getValuePaidToCreator = Ada.fromValue $
      Contexts.valuePaidTo txInfo (Ledger.unPaymentPubKeyHash $ creator benp)

    compareValues :: Ada.Ada -> Maybe Ada.Ada -> Bool
    compareValues vToCreator adaTx = Just vToCreator >= adaTx

    -- Check if transaction pays creator enough royalties
    calculateRoyalties :: Bool
    calculateRoyalties = compareValues getValuePaidToCreator adaRoyalties


-- --------------------------------------------------------------------------------
-- Boilerplate

simpleTypeV :: BenParam -> V2UtilsTypeScripts.TypedValidator Simple
simpleTypeV benp = V2UtilsTypeScripts.mkTypedValidator @Simple
    ($$(compile [|| simpleType ||]) `PlutusTx.applyCode` PlutusTx.liftCode benp)
     $$(compile [|| wrap ||])
  where
    wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

validator :: BenParam -> V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript . simpleTypeV

validatorHash :: BenParam -> V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash . simpleTypeV

address :: BenParam -> Ledger.Address
address = V1LAddress.scriptHashAddress . validatorHash
