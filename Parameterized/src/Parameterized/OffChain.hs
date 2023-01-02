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

module Parameterized.OffChain where

import qualified Control.Monad            as Monad (void)
import qualified GHC.Generics             as GHCGenerics (Generic)
import qualified Data.Aeson               as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema      as DataOpenApiSchema (ToSchema)
import qualified Prelude                  as P
import qualified Data.Void                as Void (Void)
import qualified Data.Map                 as Map
import Data.Text                          (Text, replicate)
import qualified Data.Text                as T
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

import qualified Parameterized.OnChain    as OnChain

-- ----------------------------------------------------------------------
-- Data types
-- ----------------------------------------------------------------------

-- Data for start action
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

-- Data for grab action
data GrabParams = GrabParams
  { gpCreator  :: !Ledger.PaymentPubKeyHash
  , gpDeadline :: !LedgerApiV2.POSIXTime
  , gpGuess    :: !Integer
  } deriving (GHCGenerics.Generic,
              DataAeson.ToJSON,
              DataAeson.FromJSON,
              DataOpenApiSchema.ToSchema)

-- ----------------------------------------------------------------------
-- Contract endpoints
-- ----------------------------------------------------------------------

type BenSchema =
    PlutusContract.Endpoint "start" StartParams
    PlutusContract..\/
    PlutusContract.Endpoint "grab" GrabParams


-- ----------------------------------------------------------------------
-- start endpoint
-- ----------------------------------------------------------------------

start :: PlutusContract.AsContractError e
      => StartParams
      -> PlutusContract.Contract w s e ()
start sp = do
    logSimple "Start of the 'start' action"
    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Simple lookups tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx
    logStart dat val p
  where
    p    = OnChain.BenParam {
             OnChain.creator     = spCreator sp,
             OnChain.beneficiary = spBeneficiary sp,
             OnChain.deadline    = spDeadline sp
           }
    dat  = OnChain.Dat {
             OnChain.dData = spGuess sp
           }
    val  = Ada.lovelaceValueOf $ spAmount sp
    tx   = Constraints.mustPayToOtherScriptWithInlineDatum
             (OnChain.validatorHash p)
             (LedgerApiV2.Datum $ PlutusTx.toBuiltinData dat) val
             
    lookups = Constraints.plutusV2OtherScript $ OnChain.validator p

-- ----------------------------------------------------------------------
-- grab endpoint
-- ----------------------------------------------------------------------

grab :: GrabParams -> PlutusContract.Contract w s Text ()
grab GrabParams{..} = do
  beneficiary <- PlutusContract.ownFirstPaymentPubKeyHash
  now         <- fst <$> PlutusContract.currentNodeClientTimeRange

  if now < gpDeadline
    then PlutusContract.logInfo @P.String $
           printf "Deadline not yet reached - Deadline: %s - Now %s"
           (P.show gpDeadline) (P.show now)
    else do
      let param = OnChain.BenParam {
            OnChain.creator     = gpCreator,
            OnChain.beneficiary = beneficiary,
            OnChain.deadline    = gpDeadline
          }
          r = OnChain.Redeem {
            OnChain.redeem = gpGuess
          }
      
      -- Finds the utxos associated to the beneficiary that have valid
      -- deadline and guess number
      maybeUtxo <- findUtxoInValidator param (OnChain.redeem r)
      case maybeUtxo of
        Nothing -> PlutusContract.logInfo @P.String $
          printf "Wrong guess %s or not deadline reached %s and pubkey %s"
            (P.show r) (P.show now) (P.show $ OnChain.beneficiary param)

        Just (oref, o) -> do
          PlutusContract.logInfo @P.String $
            printf "Redeem utxo %s - with timing now at %s:" (P.show oref) (P.show now)

          -- UTXO found (param matches)
          let lookups  = Constraints.unspentOutputs (Map.singleton oref o)          P.<>
                         Constraints.plutusV2OtherScript (OnChain.validator param)

              tx       = Constraints.mustSpendScriptOutput oref                     
                           (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r)      P.<>
                         Constraints.mustValidateIn (LedgerApiV2.from now)          P.<>
                         Constraints.mustPayToPubKey gpCreator (getTotalValuePay o) P.<>
                         Constraints.mustBeSignedBy beneficiary
          
          submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Simple lookups tx
          Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
          PlutusContract.logInfo @P.String $ "collected gifts"


-- ----------------------------------------------------------------------
-- Contract monad functions
-- ----------------------------------------------------------------------

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
-- Check a UTXO's datum against our validation criteria (datum matches)
checkUTXO :: TxOutTup -> Integer -> Bool
checkUTXO (oref, o) n =
  case getDatum (oref, o) of
    Nothing -> False
    Just OnChain.Dat{..}
      | dData == n -> True
      | otherwise  -> False


-- || findUTXO
-- Return a UTXO that satisfies our validation criteria (datum matches)
findUTXO :: [TxOutTup] -> Integer -> Maybe TxOutTup
findUTXO [] _ = Nothing
findUTXO [(oref, o)] n = do
    if checkUTXO (oref, o) n
      then return (oref, o)
      else Nothing
findUTXO ((oref, o):xs) n
    | checkUTXO (oref, o) n = return (oref, o)
    | otherwise             = findUTXO xs n


-- || findUtxoInValidator
-- Return a UTXO at validator address that satisfies our validation criteria
findUtxoInValidator :: OnChain.BenParam -> Integer -> PlutusContract.Contract w s Text (Maybe TxOutTup)
findUtxoInValidator bp n = do
  utxos <- PlutusContract.utxosAt (OnChain.address bp)
  PlutusContract.logInfo @P.String $ printf "utxos found: %d" (P.length utxos)

  let xs  = [(oref, o) | (oref, o) <- Map.toList utxos]
      out = findUTXO xs n
  return out


-- || getTotalValuePay
-- Divide total ADA in the UTXO by 10 (calculate royalties)
getTotalValuePay :: Ledger.DecoratedTxOut -> Ledger.Value
getTotalValuePay o =
  Ada.toValue $ Ada.fromValue (Ledger._decoratedTxOutValue o) `Ada.divide` 10

-- ----------------------------------------------------------------------
-- Endpoints
-- ----------------------------------------------------------------------

endpoints :: PlutusContract.Contract () BenSchema Text ()
endpoints = do
    PlutusContract.awaitPromise (start' `PlutusContract.select` grab')
    endpoints
  where
    start' = PlutusContract.endpoint @"start" start
    grab'  = PlutusContract.endpoint @"grab"  grab

-- ----------------------------------------------------------------------
-- Logging utils
-- ----------------------------------------------------------------------

-- log simple message 
logSimple 
    :: P.String
    -> PlutusContract.Contract w s e ()
logSimple str = PlutusContract.logInfo @P.String $ printf str


-- log for start action
logStart
    :: OnChain.Dat
    -> Ledger.Value
    -> OnChain.BenParam
    -> PlutusContract.Contract w s e ()
logStart d v p = PlutusContract.logInfo @P.String $ printf full
  where
    scriptAddress = OnChain.address p
    scriptHash    = OnChain.validatorHash p
    dashes        = P.replicate 27 '-'
    full          = dashes
      P.<> " Start Endpoint - Submitted - Datum " P.<> P.show d
      P.<> " - Value " P.<> P.show v P.<> "\n"
      P.<> dashes P.<> " Script with Address: " P.<> P.show scriptAddress
      P.<> " and Hash: " P.<> P.show scriptHash
