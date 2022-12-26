{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Homework2 where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data(..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency(..))
import           Prelude              (IO, Semigroup(..), String, show)
import           Text.Printf          (printf)
import           Ledger.Address       (scriptValidatorHashAddress)

-- ----------------------------------------------------------------------
-- Data types
-- ----------------------------------------------------------------------

data MyRedeemer = MyRedeemer
  { flag1 :: Bool
  , flag2 :: Bool
  } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''MyRedeemer

-- ----------------------------------------------------------------------
-- On-chain
-- ----------------------------------------------------------------------

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> MyRedeemer -> ScriptContext -> Bool
mkValidator _ (MyRedeemer f1 f2) _ =
  traceIfFalse "wrong redeemer" (f1 == f2)

data Typed
instance Scripts.ValidatorTypes Typed where
  type instance DatumType Typed = ()
  type instance RedeemerType Typed = MyRedeemer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
  $$(PlutusTx.compile [|| mkValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator @() @MyRedeemer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

srcAddress :: Ledger.Address
srcAddress = scriptHashAddress valHash

-- ----------------------------------------------------------------------
-- Off-chain
-- ----------------------------------------------------------------------

type GiftSchema = Endpoint "give" Integer
              .\/ Endpoint "grab" MyRedeemer


-- give endpoint
give :: forall w s e. AsContractError e => Integer -> Contract w s e ()
give amount = do
  let tx = mustPayToTheScriptWithDatumHash () $ Ada.lovelaceValueOf amount
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed (getCardanoTxId ledgerTx)
  logInfo @String $ printf "made gift of $d lovelace" amount


-- grab endpoint
grab :: forall w s e. AsContractError e => MyRedeemer -> Contract w s e ()
grab r = do
  utxos <- utxosAt srcAddress
  let orefs = fst <$> Map.toList utxos
      lookups = Constraints.unspentOutputs utxos <>
                Constraints.plutusV2OtherScript validator
             
      tx :: TxConstraints Void Void
      tx = mconcat [ mustSpendScriptOutput oref
             (Redeemer $ PlutusTx.toBuiltinData r)
             | oref <- orefs ]
           
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed (getCardanoTxId ledgerTx)
  logInfo @String $ "collected gifts"


-- endpoints
endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

