{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module FortyTwo where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency(..))
import           Prelude             (IO, Semigroup(..), String, show)
import           Text.Printf         (printf)
import           Ledger.Address      (scriptValidatorHashAddress)
import qualified Plutus.Script.Utils.V2.Scripts as V2Scripts

-- ----------------------------------------------------------------------
-- On-chain 
-- ----------------------------------------------------------------------

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ r _
  | r == Builtins.mkI 42 = ()
  | otherwise            = traceError "Incorrect Redeemer"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: Ledger.ValidatorHash
valHash = V2Scripts.validatorHash validator

srcAddress :: Ledger.Address
srcAddress = scriptHashAddress valHash

-- ----------------------------------------------------------------------
-- Off-chain 
-- ----------------------------------------------------------------------

type GiftSchema = Endpoint "give" Integer
              .\/ Endpoint "grab" Integer


-- give endpoint
give :: forall w s e. AsContractError e => Integer -> Contract w s e ()
give amount = do
  let tx = mustPayToOtherScriptWithDatumHash
             valHash                      -- validator hash 
             (Datum $ Builtins.mkI 0) $   -- datum (arbitrary)
             Ada.lovelaceValueOf amount   -- lovelace to send
  ledgerTx <- submitTx tx
  void $ awaitTxConfirmed (getCardanoTxId ledgerTx)
  logInfo @String $ printf "made gift of $d lovelace" amount


-- grab endpoint
grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab redeemer = do
  utxos <- utxosAt srcAddress
  let orefs = fst <$> Map.toList utxos
      lookups = Constraints.unspentOutputs utxos
             <> Constraints.plutusV2OtherScript validator
      tx :: TxConstraints Void Void
      tx = mconcat [ mustSpendScriptOutput oref           -- must spend output
                       (Redeemer $ Builtins.mkI redeemer) -- passed redeember
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

mkSchemaDefinitions ''GiftSchema
mkKnownCurrencies []
