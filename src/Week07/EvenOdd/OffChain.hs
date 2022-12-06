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

module Week07.EvenOdd.OffChain where

import Control.Monad                       (void)
import qualified GHC.Generics              as GHCGenerics (Generic)
import qualified Data.Aeson                as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema       as DataOpenApiSchema (ToSchema)

import qualified Prelude                   as P
import Data.Void                           (Void)
import qualified Data.Map                  as Map
import Data.Maybe                          (fromJust, maybe)
import Data.Text                           (Text)
import qualified Data.Text                 as T
import Text.Printf                         (printf)

import qualified PlutusTx                  
import PlutusTx.Prelude                    
import qualified Plutus.Contract           as PlutusContract
import qualified Ledger.Ada                as Ada
import qualified Ledger.Tx                 as LedgerTx
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Ledger
import qualified Ledger.Constraints        as Constraints
import qualified Plutus.V1.Ledger.Scripts  as ScriptsLedger
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Plutus.V1.Ledger.Interval as LedgerInterval 
import qualified Ledger.Address            as LAddressV1

import qualified Week07.EvenOdd.OnChain    as OnChain


-- ----------------------------------------------------------------------
-- Utility functions

-- Return the script's correct on-chain utxo data
findGameOutput
    :: OnChain.Game
    -> PlutusContract.Contract w s Text (
         Maybe (Ledger.TxOutRef, Ledger.DecoratedTxOut, OnChain.GameDatum))
findGameOutput game = do
    utxos <- PlutusContract.utxosAt $ OnChain.gameAddress game
    PlutusContract.logInfo @P.String $ printf "UTXOs found: %d" (P.length utxos)
    PlutusContract.logInfo @P.String $ printf "UTXO: %s" (P.show utxos)
    
    case find f $ Map.toList utxos of
      Nothing        -> return Nothing
      Just (oref, o) -> do
        let mDat = case snd $ Ledger._decoratedTxOutScriptDatum o of
                     Ledger.DatumUnknown  -> Nothing
                     Ledger.DatumInline d -> PlutusTx.fromBuiltinData $ Ledger.getDatum d
                     Ledger.DatumInBody d -> PlutusTx.fromBuiltinData $ Ledger.getDatum d

        case mDat of
          Nothing -> return Nothing
          Just d' -> return $ Just (oref, o, d')
  where
    f :: (Ledger.TxOutRef, Ledger.DecoratedTxOut) -> Bool
    f (_, o) = Value.assetClassValueOf (Ledger._decoratedTxOutValue o) (OnChain.gToken game) == 1


-- Wait until a provided POSIX time has passed
waitUntilTimeHasPassed
    :: PlutusContract.AsContractError e
    => LedgerApiV2.POSIXTime
    -> PlutusContract.Contract w s e ()
waitUntilTimeHasPassed t = do
  s1 <- PlutusContract.currentNodeClientSlot
  PlutusContract.logInfo @P.String $ printf "current slot: %s, waiting until: %s" (P.show s1) (P.show t)
  void $ PlutusContract.awaitTime t >> PlutusContract.waitNSlots 2
  s2 <- PlutusContract.currentNodeClientSlot
  PlutusContract.logInfo @P.String $ printf "waited until: %s" (P.show s2)


-- ----------------------------------------------------------------------
-- First game 

data FirstParams = FirstParams
  { fpSecond         :: !LAddressV1.PaymentPubKeyHash
  , fpStake          :: !Integer
  , fpPlayDeadline   :: !LedgerApiV2.POSIXTime
  , fpRevealDeadline :: !LedgerApiV2.POSIXTime
  , fpNonce          :: !LedgerApiV2.BuiltinByteString
  , fpCurrency       :: !LedgerApiV2.CurrencySymbol
  , fpTokenName      :: !LedgerApiV2.TokenName
  , fpChoice         :: !OnChain.GameChoice
  } deriving (P.Show,
              GHCGenerics.Generic,
              DataAeson.FromJSON,
              DataAeson.ToJSON,
              DataOpenApiSchema.ToSchema)


firstGame :: forall w s. FirstParams -> PlutusContract.Contract w s Text ()
firstGame fp = do
  pkh <- PlutusContract.ownFirstPaymentPubKeyHash
  let game = OnChain.Game {
               OnChain.gFirst          = pkh,
               OnChain.gSecond         = fpSecond fp,
               OnChain.gStake          = fpStake fp,
               OnChain.gPlayDeadline   = fpPlayDeadline fp,
               OnChain.gRevealDeadline = fpRevealDeadline fp,
               OnChain.gToken          = Value.AssetClass (fpCurrency fp, fpTokenName fp)
             }
      v  = Ada.lovelaceValueOf (fpStake fp) <> Value.assetClassValue (OnChain.gToken game) 1
      c  = fpChoice fp
      bs = sha2_256 $ fpNonce fp `appendByteString`
             if c == OnChain.Zero then OnChain.bsZero else OnChain.bsOne
      d = OnChain.GameDatum bs Nothing

      -- Construct initial UTXO
      lookups = Constraints.typedValidatorLookups (OnChain.typedGameValidator game)
      tx      = Constraints.mustPayToTheScriptWithInlineDatum d v
                
  -- Submit initial UTXO
  ledgerTx <- PlutusContract.submitTxConstraintsWith @OnChain.Gaming lookups tx
  void $ PlutusContract.awaitTxConfirmed (LedgerTx.getCardanoTxId ledgerTx)
  PlutusContract.logInfo @P.String $ printf "made first move: " ++ P.show (fpChoice fp)
  
  waitUntilTimeHasPassed $ fpPlayDeadline fp

  m   <- findGameOutput game
  now <- fst <$> PlutusContract.currentNodeClientTimeRange

  PlutusContract.logInfo @P.String $ printf "found output: %s" (P.show m)

  case m of
    -- Script utxo not found
    Nothing -> PlutusContract.throwError "game output not found"

    -- Script utxo found
    Just (oref, o, dat) -> case dat of

      -- Second player did not play - ClaimFirst
      OnChain.GameDatum _ Nothing -> do
        PlutusContract.logInfo @P.String "second player did not play"
        let lookups' = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                       Constraints.typedValidatorLookups (OnChain.typedGameValidator game)
                       
            tx'      = Constraints.mustSpendScriptOutput oref
                         (LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData OnChain.ClaimFirst) P.<>
                       Constraints.mustValidateIn (LedgerInterval.from now) P.<>
                       Constraints.mustBeSignedBy pkh
                       
        ledgerTx' <- PlutusContract.submitTxConstraintsWith @OnChain.Gaming lookups' tx'
        void $ PlutusContract.awaitTxConfirmed (LedgerTx.getCardanoTxId ledgerTx')
        PlutusContract.logInfo @P.String "reclaimed stake"

      -- Second player played and lost - Reveal
      OnChain.GameDatum _ (Just c') | c' == c -> do
        PlutusContract.logInfo @P.String "second player played and lost"
        let lookups'' = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                        Constraints.typedValidatorLookups (OnChain.typedGameValidator game)
                        
            tx''      = Constraints.mustSpendScriptOutput oref
                          (LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Reveal $ fpNonce fp) P.<>
                        Constraints.mustValidateIn (LedgerInterval.to $ now + 1000) P.<>
                        Constraints.mustBeSignedBy pkh
                        
        ledgerTx'' <- PlutusContract.submitTxConstraintsWith @OnChain.Gaming lookups'' tx''
        void $ PlutusContract.awaitTxConfirmed (LedgerTx.getCardanoTxId ledgerTx'')
        PlutusContract.logInfo @P.String "victory"

      -- Otherwise, the second player played and won
      _ -> PlutusContract.logInfo @P.String "second player played and won"


-- ----------------------------------------------------------------------
-- Second game 

data SecondParams = SecondParams
  { spFirst          :: !LAddressV1.PaymentPubKeyHash
  , spStake          :: !Integer
  , spPlayDeadline   :: !LedgerApiV2.POSIXTime
  , spRevealDeadline :: !LedgerApiV2.POSIXTime
  , spCurrency       :: !LedgerApiV2.CurrencySymbol
  , spTokenName      :: !LedgerApiV2.TokenName
  , spChoice         :: !OnChain.GameChoice
  } deriving (P.Show,
              GHCGenerics.Generic,
              DataAeson.FromJSON,
              DataAeson.ToJSON,
              DataOpenApiSchema.ToSchema)


secondGame :: forall w s. SecondParams -> PlutusContract.Contract w s Text ()
secondGame sp = do
  pkh <- PlutusContract.ownFirstPaymentPubKeyHash
  let game = OnChain.Game {
               OnChain.gFirst          = spFirst sp,
               OnChain.gSecond         = pkh,
               OnChain.gStake          = spStake sp,
               OnChain.gPlayDeadline   = spPlayDeadline sp,
               OnChain.gRevealDeadline = spRevealDeadline sp,
               OnChain.gToken          = Value.AssetClass (spCurrency sp, spTokenName sp)
             }

  m <- findGameOutput game
  case m of
    Just (oref, o, OnChain.GameDatum bs Nothing) -> do
      PlutusContract.logInfo @P.String "running game found"
      
      now <- snd <$> PlutusContract.currentNodeClientTimeRange
      let token' = Value.assetClassValue (OnChain.gToken game) 1
          v      = let x = Ada.lovelaceValueOf (spStake sp) in x <> x <> token'
          c      = spChoice sp

          -- Construct Play transaction
          lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                    Constraints.typedValidatorLookups (OnChain.typedGameValidator game)
                    --Constraints.plutusV1TypedValidatorLookups (OnChain.typedGameValidator game)
                    
          tx      = Constraints.mustSpendScriptOutput oref
                      (LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Play c) P.<>
                    Constraints.mustPayToTheScriptWithInlineDatum (OnChain.GameDatum bs $ Just c) v   P.<>
                    Constraints.mustValidateIn (LedgerInterval.to now) P.<>
                    Constraints.mustBeSignedBy pkh

      -- Submit Play transaction
      ledgerTx <- PlutusContract.submitTxConstraintsWith @OnChain.Gaming lookups tx
      void $ PlutusContract.awaitTxConfirmed (LedgerTx.getCardanoTxId ledgerTx)
      PlutusContract.logInfo @P.String $ printf "made second move: " ++ P.show (spChoice sp)

      -- Wait until reveal deadline passed
      waitUntilTimeHasPassed $ spRevealDeadline sp

      -- See if a UTXO still exists (Player 1 hasn't revealed)
      m'   <- findGameOutput game
      now' <- fst <$> PlutusContract.currentNodeClientTimeRange

      case m' of
        -- Script utxo not found - Player 1 must have spent it
        Nothing -> PlutusContract.logInfo @P.String "first player won"

        -- Script utxo found - Player 1 hasn't revealed - ClaimSecond
        Just (oref', o', _) -> do
          PlutusContract.logInfo @P.String "first player didn't reveal"

          -- Construct ClaimSecond transaction
          let lookups' = Constraints.unspentOutputs (Map.singleton oref' o') P.<>
                         Constraints.typedValidatorLookups (OnChain.typedGameValidator game)
                         
              tx'      = Constraints.mustSpendScriptOutput oref'
                           (LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData OnChain.ClaimSecond) P.<>
                         Constraints.mustValidateIn (LedgerInterval.from now') P.<>
                         Constraints.mustPayToPubKey (spFirst sp)
                           (token' <> Ada.adaValueOf (Ada.getAda Ledger.minAdaTxOut)) P.<>
                         Constraints.mustBeSignedBy pkh

          -- Submit ClaimSecond transaction
          ledgerTx' <- PlutusContract.submitTxConstraintsWith @OnChain.Gaming lookups' tx'
          void $ PlutusContract.awaitTxConfirmed (LedgerTx.getCardanoTxId ledgerTx')
          PlutusContract.logInfo @P.String "second player won"

    -- Script UTXO not found
    _ -> PlutusContract.logInfo @P.String "no running game found"


-- ----------------------------------------------------------------------
-- Schema and endpoints

type GameSchema = PlutusContract.Endpoint "first" FirstParams
                  PlutusContract..\/
                  PlutusContract.Endpoint "second" SecondParams

endpoints :: PlutusContract.Contract () GameSchema Text ()
endpoints = PlutusContract.awaitPromise (first `PlutusContract.select` second) >> endpoints
  where
    first  = PlutusContract.endpoint @"first"  firstGame
    second = PlutusContract.endpoint @"second" secondGame 
