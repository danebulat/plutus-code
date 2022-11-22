{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Guess.OffChain where

import qualified Control.Monad             as Monad (void, unless, when)
import qualified GHC.Generics              as GHCGenerics (Generic)
import qualified Data.Aeson                as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema       as DataOpenApiSchema (ToSchema)
import qualified Prelude                   as P
import Data.Void                           (Void)
import qualified Data.Map                  as Map
import Data.Text                           (Text, replicate)
import Text.Printf                         (printf)

import qualified PlutusTx                  
import PlutusTx.Prelude                    
import qualified Plutus.Contract           as PlutusContract
import qualified Ledger.Ada                as Ada
import qualified Ledger.Tx                 as LedgerTx
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Ledger                    (PaymentPubKeyHash, getCardanoTxId, unitRedeemer)
import qualified Ledger.Constraints        as Constraints
import qualified Plutus.V1.Ledger.Scripts  as ScriptsLedger
import qualified Plutus.V1.Ledger.Interval as LedgerIntervalV1

import qualified Guess.OnChain             as OnChain


-- ----------------------------------------------------------------------
-- Data types 

data GiveParams = GiveParams
  { giveAmount :: !Integer
  , giveDat    :: !Integer
  } deriving (P.Eq,
              P.Ord,
              GHCGenerics.Generic,
              DataAeson.ToJSON,
              DataAeson.FromJSON,
              P.Show)

newtype GrabParams = GrabParams
  { grabRedeem :: Integer
  } deriving (P.Eq,
              P.Ord,
              GHCGenerics.Generic,
              DataAeson.ToJSON,
              DataAeson.FromJSON,
              P.Show)

-- ----------------------------------------------------------------------
-- Schema

type GiftSchema = PlutusContract.Endpoint "give" GiveParams
                  PlutusContract..\/
                  PlutusContract.Endpoint "grab" GrabParams


-- ----------------------------------------------------------------------
-- Give endpoint

give
    :: forall w s e. PlutusContract.AsContractError e
    => GiveParams
    -> PlutusContract.Contract w s e ()
give gp = do
  PlutusContract.logInfo @P.String $ printf
    "------------------------------------------------------"
  PlutusContract.logInfo @P.String $ printf
    "----------------Give endpoint initialize--------------"
  PlutusContract.logInfo @P.String $ printf
    "------------------------------------------------------"

  let d = OnChain.Dat { OnChain.dData = giveDat gp }
      q = giveAmount gp

      lookups = Constraints.plutusV2OtherScript OnChain.validator

      tx = Constraints.mustPayToOtherScript OnChain.validatorHash
        (ScriptsLedger.Datum $ PlutusTx.toBuiltinData d)
        (Ada.lovelaceValueOf q)

  submittedTx <- PlutusContract.submitTxConstraintsWith @Void lookups tx
  Monad.void $ PlutusContract.awaitTxConfirmed (Ledger.getCardanoTxId submittedTx)
  PlutusContract.logInfo @P.String $
    printf "Made transaction of %d ADA" q


-- ----------------------------------------------------------------------
-- Grab endpoint

grab :: GrabParams -> PlutusContract.Contract w s Text ()
grab GrabParams{..} = do

  maybeUtxo <- findUtxoInValidator grabRedeem

  case maybeUtxo of
    Nothing -> PlutusContract.logInfo @P.String $ printf
                 "Wrong guess %d" grabRedeem
    Just (oref, o) -> do
      PlutusContract.logInfo @P.String $ printf
        "Redeem utxo %s" (P.show oref)

      let r = OnChain.Redeem { OnChain.redeem = grabRedeem }

          lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                    Constraints.plutusV2OtherScript OnChain.validator

          tx = Constraints.mustSpendScriptOutput oref
                 (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r)

      submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.VTypes lookups tx
      Monad.void $ PlutusContract.awaitTxConfirmed (LedgerTx.getCardanoTxId submittedTx)
      PlutusContract.logInfo @P.String "Collected gifts"


-- ----------------------------------------------------------------------
-- Helper function

type TxOutTup = (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)


getDatum :: TxOutTup -> Maybe OnChain.Dat
getDatum (_, o) = do
    let datHashOrDatum = LedgerTx._ciTxOutScriptDatum o
    LedgerApiV2.Datum builtInData <- snd datHashOrDatum
    case (LedgerApiV2.fromBuiltinData builtInData :: Maybe OnChain.Dat) of
        Nothing -> Nothing
        dat -> dat


checkUTXO :: TxOutTup -> Integer -> Bool
checkUTXO (oref, o) n =
  case getDatum (oref, o) of
    Nothing -> False
    Just OnChain.Dat{..}
      | dData == n -> True
      | otherwise  -> False


findUTXO :: [TxOutTup] -> Integer -> Maybe TxOutTup
findUTXO [] _ = Nothing
findUTXO [(oref, o)] n = do
    if checkUTXO (oref, o) n
      then return (oref, o)
      else Nothing
findUTXO ((oref, o):xs) n
    | checkUTXO (oref, o) n = return (oref, o)
    | otherwise = findUTXO xs n


findUtxoInValidator :: Integer -> PlutusContract.Contract w s Text (Maybe TxOutTup)
findUtxoInValidator n = do
  utxos <- PlutusContract.utxosAt OnChain.address
  let xs = [(oref, o) | (oref, o) <- Map.toList utxos]
      out = findUTXO xs n
  return out


-- ----------------------------------------------------------------------
-- Endpoints 

endpoints :: PlutusContract.Contract () GiftSchema Text ()
endpoints = do
    PlutusContract.awaitPromise (give' `PlutusContract.select` grab')
    endpoints
  where
     give' = PlutusContract.endpoint @"give" give
     grab' = PlutusContract.endpoint @"grab" grab
