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

module Week02.Burn where

import           Control.Monad                  hiding (fmap)
import           Data.Map                       as Map
import           Data.Text                      (Text)
import           Data.Void                      (Void)
import           Plutus.Contract                
import           PlutusTx                       
import qualified PlutusTx                       
import qualified PlutusTx.Builtins              as Builtins
import qualified Plutus.Script.Utils.V2.Scripts as V2Scripts
import           PlutusTx.Prelude               hiding (Semigroup(..), unless)
import           Ledger                         hiding (singleton)
import           Ledger.Constraints             as Constraints
import qualified Ledger.Scripts                 as Scripts
import           Ledger.Ada                     as Ada
import           Playground.Contract            (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH                  (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types               (KnownCurrency(..))
import           Prelude                        (IO, Semigroup(..), String, show)
import           Text.Printf                    (printf)
import           Ledger.Address                 (scriptValidatorHashAddress)
                                                


-- ----------------------------------------------------------------------
-- On-chain 

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = traceError "BURNT!"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: Ledger.ValidatorHash
valHash = V2Scripts.validatorHash validator

srcAddress :: Ledger.Address
srcAddress = scriptHashAddress valHash

-- ----------------------------------------------------------------------
-- Off-chain 

type GiftSchema = Endpoint "give" Integer
              .\/ Endpoint "grab" ()


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
grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
  utxos <- utxosAt srcAddress
  let orefs = fst <$> Map.toList utxos
      lookups = Constraints.unspentOutputs utxos
             <> Constraints.plutusV2OtherScript validator
      tx :: TxConstraints Void Void
      tx = mconcat [ mustSpendScriptOutput oref      -- must spend output
                       (Redeemer $ Builtins.mkI 17)  -- arbitrary redeember
                     | oref <- orefs ]
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed (getCardanoTxId ledgerTx)
  logInfo @String $ "collected gifts"


-- endpoints
endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" (const grab)

mkSchemaDefinitions ''GiftSchema
mkKnownCurrencies []
