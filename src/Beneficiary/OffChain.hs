{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-} -- To allow notation like GrabParams {..}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Beneficiary.OffChain where

import qualified Control.Monad            as Monad (void)
import qualified GHC.Generics             as GHCGenerics (Generic)
import qualified Data.Aeson               as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema      as DataOpenApiSchema (ToSchema)
import qualified Prelude                  as P
import qualified Data.Void                as Void (Void)
import qualified Data.Map                 as Map
import Data.Text                          (Text)
import Text.Printf                        (printf)
                                          
import qualified PlutusTx                 
import PlutusTx.Prelude                   
import qualified Plutus.Contract          as PlutusContract
import qualified Ledger.Ada               as Ada
import qualified Ledger.Tx                as LedgerTx
import qualified Plutus.V2.Ledger.Api     as LedgerApiV2
import qualified Ledger
import qualified Ledger.Constraints       as Constraints
import qualified Plutus.V1.Ledger.Scripts as ScriptsLedger

import qualified Beneficiary.OnChain as OnChain
import Playground.Types (PayToWalletParams(payTo))
import Vesting.OffChain (GiveParams(gpBeneficiary))


-- ----------------------------------------------------------------------
-- Data types

data StartParams = StartParams
  { spCreator     :: !Ledger.PaymentPubKeyHash
  , spBeneficiary :: !Ledger.PaymentPubKeyHash
  , spDeadline    :: !LedgerApiV2.POSIXTime
  , spGuess       :: !Integer
  , spAmount      :: !Integer
  } deriving (GHCGenerics.Generic,
              DataAeson.ToJSON,
              DataAeson.FromJSON,
              DataOpenApiSchema.ToSchema)

data GrabParams = GrabParams
  { grabRedeem :: !Integer
  } deriving (GHCGenerics.Generic,
              DataAeson.ToJSON,
              DataAeson.FromJSON,
              DataOpenApiSchema.ToSchema)

-- ----------------------------------------------------------------------
-- Contract endpoints

type GiftSchema =
    PlutusContract.Endpoint "start" StartParams
    PlutusContract..\/
    PlutusContract.Endpoint "grab" GrabParams


-- start endpoint
start :: PlutusContract.AsContractError e
      => StartParams
      -> PlutusContract.Contract w s e ()
start sp = do
    PlutusContract.logInfo @P.String $ printf "Start of the give action"
    submittedTx <- PlutusContract.submitTxConstraintsWith @Void.Void lookups tx
    
    Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ printf "Made transaction of %d lovelace"
      (Ada.getLovelace $ Ada.fromValue val)
  where
    d  = OnChain.Dat { OnChain.creator     = spCreator sp
                     , OnChain.beneficiary = spBeneficiary sp
                     , OnChain.deadline    = spDeadline sp
                     , OnChain.dData       = spGuess sp
                     }
    val = Ada.lovelaceValueOf $ spAmount sp
    tx  = Constraints.mustPayToOtherScriptWithInlineDatum OnChain.validatorHash
           (ScriptsLedger.Datum $ PlutusTx.toBuiltinData d) val
    lookups = Constraints.plutusV2OtherScript OnChain.validator


-- grab endpoint
grab :: GrabParams -> PlutusContract.Contract w s Text ()
grab GrabParams{..} = do
  pkh       <- PlutusContract.ownFirstPaymentPubKeyHash
  now       <- fst <$> PlutusContract.currentNodeClientTimeRange
  maybeUtxo <- findUtxoInValidator pkh grabRedeem now
  
  case maybeUtxo of
    Nothing -> PlutusContract.logInfo @P.String $ printf "Invalid (%d)" grabRedeem

    Just (oref, o) -> do
      PlutusContract.logInfo @P.String $ printf "Redeem utxo %s" (P.show oref)
      case getDatum (oref, o) of
        Nothing  -> PlutusContract.logInfo @P.String $ printf "Datum not found"
        Just dat -> do
          let r        = OnChain.Redeem { OnChain.redeem = grabRedeem }
              lookups  = Constraints.unspentOutputs (Map.singleton oref o)     P.<>
                         Constraints.plutusV2OtherScript OnChain.validator

              tx       = Constraints.mustSpendScriptOutput oref
                           (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
                         Constraints.mustValidateIn (LedgerApiV2.from now)     P.<>
                         Constraints.mustPayToPubKey (OnChain.creator dat)
                           (getTotalValuePay o)                                P.<>
                         Constraints.mustBeSignedBy (OnChain.beneficiary dat)

          submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Simple lookups tx
          Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
          PlutusContract.logInfo @P.String $ "collected gifts"


-- ----------------------------------------------------------------------
-- Contract monad functions

type TxOutTup = (Ledger.TxOutRef, Ledger.DecoratedTxOut)

-- || getDatum
-- Return the datum of a passed in transaction output
getDatum :: TxOutTup -> Maybe OnChain.Dat
getDatum (_, o) = do
    let scriptDat = Ledger._decoratedTxOutScriptDatum o
    case snd scriptDat of
      Ledger.DatumUnknown  -> Nothing
      Ledger.DatumInline d -> PlutusTx.fromBuiltinData (Ledger.getDatum d)
      Ledger.DatumInBody d -> PlutusTx.fromBuiltinData (Ledger.getDatum d)

-- || checkUTXO
-- Check a UTXO's datum against our validation criteria
-- Datum matches, tx is by the beneficiary, deadline passed
checkUTXO
    :: TxOutTup
    -> Ledger.PaymentPubKeyHash
    -> Integer
    -> LedgerApiV2.POSIXTime
    -> Bool
checkUTXO (oref, o) pkh n now = do
  case getDatum (oref, o) of
    Nothing -> False
    Just OnChain.Dat{..}
      | dData == n && beneficiary == pkh && now >= deadline -> True
      | otherwise -> False


-- || findUTXO
-- Return a UTXO that satisfies our validation criteria
--
-- Parameters:
--   1. List of UTXOs at the validator script address
--   2. Pkh of tx signer
--   3. Redeemer integer (to check against datum magic number)
--   4. Current time
--
-- Returns:
--   1. The matching UTXO or Nothing

findUTXO
    :: [TxOutTup]
    -> Ledger.PaymentPubKeyHash
    -> Integer
    -> LedgerApiV2.POSIXTime
    -> Maybe TxOutTup
findUTXO [] _ _ _ = Nothing
findUTXO ((oref, o) : xs) pkh n now
  | checkUTXO (oref, o) pkh n now = return (oref, o)
  | otherwise = findUTXO xs pkh n now


-- || findUtxoInValidator
-- Return a UTXO at validator address that satisfies our validation criteria
findUtxoInValidator
    :: Ledger.PaymentPubKeyHash
    -> Integer
    -> LedgerApiV2.POSIXTime
    -> PlutusContract.Contract w s Text (Maybe TxOutTup)
findUtxoInValidator pkh n now = do
  utxos <- PlutusContract.utxosAt OnChain.address
  let xs  = [(oref, o) | (oref, o) <- Map.toList utxos]
      out = findUTXO xs pkh n now
  return out

-- || getTotalValuePay
-- Divide total ADA in the UTXO by 10 (calculate royalties)
getTotalValuePay :: Ledger.DecoratedTxOut -> Ledger.Value
getTotalValuePay o =
  Ada.toValue $ Ada.fromValue (Ledger._decoratedTxOutValue o) `Ada.divide` 10


-- ----------------------------------------------------------------------
-- Endpoints

endpoints :: PlutusContract.Contract () GiftSchema Text ()
endpoints = do
    PlutusContract.awaitPromise (start' `PlutusContract.select` grab')
    endpoints
  where
    start' = PlutusContract.endpoint @"start" start
    grab'  = PlutusContract.endpoint @"grab"  grab

