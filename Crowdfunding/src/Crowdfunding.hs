{-# LANGUAGE DataKinds             #-} 
{-# LANGUAGE TemplateHaskell       #-} 
{-# LANGUAGE NoImplicitPrelude     #-} 
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE ImportQualifiedPost   #-} 

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Crowdfunding where

import Prelude                                         qualified as P
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as V2UtilsTypeScripts
import Plutus.V2.Ledger.Contexts                       qualified as LV2Ctx
import Plutus.V2.Ledger.Api                            qualified as LV2
import Plutus.V1.Ledger.Address                        qualified as LV1Add
import Plutus.V1.Ledger.Credential                     qualified as LV1Cred
import Plutus.V1.Ledger.Interval                       qualified as LV1Int
import Ledger                                          qualified as L 
import PlutusTx                                        qualified 
import PlutusTx.Prelude
import Ledger.Ada                                      qualified as Ada

-- ---------------------------------------------------------------------- 
-- Parameter 
-- ---------------------------------------------------------------------- 

data CrowdParam = CrowdParam 
  { beneficiary  :: L.PaymentPubKeyHash 
  , targetAmount :: Ada.Ada 
  , deadline     :: LV2.POSIXTime
  }
  deriving P.Show

PlutusTx.makeIsDataIndexed ''CrowdParam [('CrowdParam, 0)]
PlutusTx.makeLift ''CrowdParam

-- ---------------------------------------------------------------------- 
-- Datum
-- ---------------------------------------------------------------------- 

newtype Dat = Dat
  { contributor :: L.PaymentPubKeyHash 
  }
  deriving P.Show

PlutusTx.makeIsDataIndexed ''Dat [('Dat, 0)]

-- ---------------------------------------------------------------------- 
-- Redeemer
-- ---------------------------------------------------------------------- 

data CrowdRedeemer
  = Collect  -- Contributor can get a refund if deadline not yet reached
  | Close    -- Beneficiary can redeem funds when deadline has passed

PlutusTx.makeIsDataIndexed ''CrowdRedeemer [ ('Collect, 0), ('Close, 1)]
PlutusTx.makeLift ''CrowdRedeemer

-- ---------------------------------------------------------------------- 
-- Validation
-- ---------------------------------------------------------------------- 

{-# INLINEABLE validation #-}
validation :: CrowdParam -> Dat -> CrowdRedeemer -> LV2Ctx.ScriptContext -> Bool
validation param d r ctx = 
    case r of 
      Collect -> traceIfFalse "deadline has already passed"   (not deadlinePassed) 
              && traceIfFalse "not signed by contributor"     signedByContributor
              && traceIfFalse "target amount reached"         (not targetAmountReached)

      Close   -> traceIfFalse "not signed by beneficiary"     signedByBeneficiary
              && traceIfFalse "deadline not yet reached"      deadlinePassed
              && traceIfFalse "target amount not yet reached" targetAmountReached 
  where 
    txInfo :: LV2Ctx.TxInfo 
    txInfo = LV2Ctx.scriptContextTxInfo ctx

    signedByBeneficiary :: Bool 
    signedByBeneficiary = LV2Ctx.txSignedBy txInfo $ L.unPaymentPubKeyHash (beneficiary param) 

    signedByContributor :: Bool 
    signedByContributor =  LV2Ctx.txSignedBy txInfo $ L.unPaymentPubKeyHash (contributor d) 

    deadlinePassed :: Bool 
    deadlinePassed = 
      LV1Int.contains (LV1Int.from (deadline param)) 
       (LV2Ctx.txInfoValidRange txInfo)

    txInfoInputs :: [LV2Ctx.TxInInfo]
    txInfoInputs = LV2Ctx.txInfoInputs txInfo

    ownInputsAdaSum :: [LV2Ctx.TxInInfo] -> Ada.Ada
    ownInputsAdaSum [] = mempty 
    ownInputsAdaSum (input:inputs) =
      let utxo = LV2Ctx.txInInfoResolved input
          adaInUtxo = Ada.fromValue $ LV2Ctx.txOutValue utxo
          -- ^ Get ADA amount in utxo
          utxoAddress = LV1Add.addressCredential $ LV2Ctx.txOutAddress utxo
          -- ^ Get associated address of the utxo of type Credential
          campaignAddress = LV1Cred.ScriptCredential $ LV2Ctx.ownHash ctx 
          -- ^ Get address of script of type Credential
      in  (if utxoAddress == campaignAddress then adaInUtxo else mempty)
            + ownInputsAdaSum inputs

    targetAmountReached :: Bool 
    targetAmountReached = ownInputsAdaSum txInfoInputs >= targetAmount param

-- ---------------------------------------------------------------------- 
-- Boilerplate
-- ---------------------------------------------------------------------- 

data Crowdfunding 
instance V2UtilsTypeScripts.ValidatorTypes Crowdfunding where
  type instance DatumType    Crowdfunding = Dat
  type instance RedeemerType Crowdfunding = CrowdRedeemer

typeValidator :: CrowdParam -> V2UtilsTypeScripts.TypedValidator Crowdfunding 
typeValidator param = V2UtilsTypeScripts.mkTypedValidator @Crowdfunding
  ($$(PlutusTx.compile [|| validation ||]) `PlutusTx.applyCode` PlutusTx.liftCode param)
   $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @CrowdRedeemer

validator :: CrowdParam -> LV2.Validator 
validator = V2UtilsTypeScripts.validatorScript . typeValidator

typeValidatorHash :: CrowdParam -> LV2.ValidatorHash 
typeValidatorHash = V2UtilsTypeScripts.validatorHash . typeValidator

crowdfundAddress :: CrowdParam -> LV2.Address
crowdfundAddress = V2UtilsTypeScripts.validatorAddress . typeValidator

