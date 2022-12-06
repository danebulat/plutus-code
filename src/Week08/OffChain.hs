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

module Week08.OffChain where


import Control.Monad                       (void)
import qualified GHC.Generics              as GHCGenerics (Generic)
import qualified Data.Aeson                as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema       as DataOpenApiSchema (ToSchema)

import qualified Prelude                   as P
import Data.Void                           (Void)
import qualified Data.Map                  as Map
import Data.Maybe                          (fromJust, maybe)
import Data.Monoid                         (Last(..))
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

import qualified Week08.OnChain            as OnChain


-- ----------------------------------------------------------------------
-- Helper Functions

findTsOutput
    :: OnChain.TokenSale
    -> PlutusContract.Contract w s Text
         (Maybe (Ledger.TxOutRef, Ledger.DecoratedTxOut, Integer))
findTsOutput ts = do
  utxos <- PlutusContract.utxosAt $ OnChain.tsScriptAddress ts

  case find hasThreadToken $ Map.toList utxos of
    Nothing -> return Nothing
    Just (oref, o) -> do
      case getDat o of
        Nothing -> return Nothing
        Just d' -> return $ Just (oref, o, d')
  where
    hasThreadToken :: (Ledger.TxOutRef, Ledger.DecoratedTxOut) -> Bool
    hasThreadToken (_, o) =
      Value.assetClassValueOf
        (Ledger._decoratedTxOutValue o) (OnChain.tsTT ts) == 1

    getDat :: PlutusTx.FromData a => Ledger.DecoratedTxOut -> Maybe a
    getDat o = case snd $ Ledger._decoratedTxOutScriptDatum o of
      Ledger.DatumUnknown  -> Nothing
      Ledger.DatumInline d -> PlutusTx.fromBuiltinData $ Ledger.getDatum d
      Ledger.DatumInBody d -> PlutusTx.fromBuiltinData $ Ledger.getDatum d


-- ----------------------------------------------------------------------
-- Action: Start Token Sale 

startTS :: OnChain.TokenSale -> PlutusContract.Contract (Last OnChain.TokenSale) s Text ()
startTS ts = do
  let
    dat     = 0
    lookups = Constraints.typedValidatorLookups (OnChain.tsTypedValidator ts)
    tx      = Constraints.mustPayToTheScriptWithInlineDatum dat mempty P.<>
              Constraints.mustBeSignedBy (OnChain.tsSeller ts)

  -- Submit tx to produce initial UTXO
  ledgerTx <- PlutusContract.submitTxConstraintsWith @OnChain.TS lookups tx
  void $ PlutusContract.awaitTxConfirmed (Ledger.getCardanoTxId ledgerTx)
  PlutusContract.logInfo @P.String "produced token sale output"


-- ----------------------------------------------------------------------
-- Action: Set Price

setPrice :: OnChain.TokenSale -> Integer -> PlutusContract.Contract w s Text ()
setPrice ts p = do
  m <- findTsOutput ts
  case m of
    Nothing -> PlutusContract.logInfo @P.String "token sale utxo not found"
    Just (oref, o, _) -> do
      PlutusContract.logInfo @P.String "token sale utxo found"

      let
        lookups =
          Constraints.unspentOutputs (Map.singleton oref o) P.<>
          Constraints.typedValidatorLookups (OnChain.tsTypedValidator ts)
                     
        tx =
          Constraints.mustSpendScriptOutput oref
            (Ledger.Redeemer $ PlutusTx.toBuiltinData $ OnChain.SetPrice p) P.<>
          Constraints.mustPayToTheScriptWithInlineDatum p
            (Ledger._decoratedTxOutValue o) P.<>
          Constraints.mustBeSignedBy (OnChain.tsSeller ts)

      -- Submit tx to set a new token price
      ledgerTx <- PlutusContract.submitTxConstraintsWith @OnChain.TS lookups tx
      void $ PlutusContract.awaitTxConfirmed (Ledger.getCardanoTxId ledgerTx)
      PlutusContract.logInfo @P.String $ printf "set token price to %d lovelace" p


-- ----------------------------------------------------------------------
-- Action: Add Tokens

addTokens :: OnChain.TokenSale -> Integer -> PlutusContract.Contract w s Text ()
addTokens ts n = do
  m <- findTsOutput ts
  case m of
    Nothing -> PlutusContract.logInfo @P.String "token sale utxo not found"
    Just (oref, o, p) -> do
      PlutusContract.logInfo @P.String "token sale utxo found"

      let
        lookups =
          Constraints.unspentOutputs (Map.singleton oref o) P.<>
          Constraints.typedValidatorLookups (OnChain.tsTypedValidator ts)

        tx =
          Constraints.mustSpendScriptOutput oref
            (Ledger.Redeemer $ PlutusTx.toBuiltinData $ OnChain.AddTokens n) P.<>
          Constraints.mustPayToTheScriptWithInlineDatum p
            (Ledger._decoratedTxOutValue o P.<>
             Value.assetClassValue (OnChain.tsToken ts) n)

      -- Submit tx to add tokens to the script
      ledgerTx <- PlutusContract.submitTxConstraintsWith @OnChain.TS lookups tx
      void $ PlutusContract.awaitTxConfirmed (Ledger.getCardanoTxId ledgerTx)
      PlutusContract.logInfo @P.String $ printf "paid %d tokens to the script" n


-- ----------------------------------------------------------------------
-- Action: Buy Tokens

buyTokens :: OnChain.TokenSale -> Integer -> PlutusContract.Contract w s Text ()
buyTokens ts n = do
  m <- findTsOutput ts
  case m of
    Nothing -> PlutusContract.logInfo @P.String "token sale utxo not found"
    Just (oref, o, p) -> do
      PlutusContract.logInfo @P.String "token sale utxo found"

      -- Check if contract holds enough tokens
      if Value.assetClassValueOf (Ledger._decoratedTxOutValue o) (OnChain.tsToken ts) <= n
        then PlutusContract.logInfo @P.String "contract doesn't hold enough tokens"
        else do
          pkh <- PlutusContract.ownFirstPaymentPubKeyHash
          
          let
            scriptOutVal =
              Ledger._decoratedTxOutValue o P.<>
              Ada.lovelaceValueOf (p * n)   P.<>
              Value.assetClassValue (OnChain.tsToken ts) (negate n)

            buyerReceiveValue =
              Value.assetClassValue (OnChain.tsToken ts) n
          
          let
            lookups =
              Constraints.unspentOutputs (Map.singleton oref o) P.<>
              Constraints.typedValidatorLookups (OnChain.tsTypedValidator ts)
            
            tx =
              Constraints.mustSpendScriptOutput oref
                (Ledger.Redeemer $ PlutusTx.toBuiltinData $ OnChain.BuyTokens n) P.<>
              Constraints.mustPayToTheScriptWithInlineDatum p scriptOutVal       P.<>
              Constraints.mustPayToPubKey pkh buyerReceiveValue                  P.<>
              Constraints.mustBeSignedBy pkh
          
          -- Submit tx to buy tokens from the contract
          ledgerTx <- PlutusContract.submitTxConstraintsWith @OnChain.TS lookups tx
          void $ PlutusContract.awaitTxConfirmed (Ledger.getCardanoTxId ledgerTx)
          PlutusContract.logInfo @P.String $
            printf "bought %d tokens for %d lovelace" n (n * p)


-- ----------------------------------------------------------------------
-- Action: Withdraw Tokens

withdraw :: OnChain.TokenSale -> Integer -> Integer -> PlutusContract.Contract w s Text ()
withdraw ts n l = do
  m <- findTsOutput ts
  case m of
    Nothing -> PlutusContract.logInfo @P.String "token sale utxo not found"
    Just (oref, o, p) -> do
      PlutusContract.logInfo @P.String "token sale utxo found"

      -- Check if contract holds enough tokens for the withdrawal
      let
        tokensInScript = 
          Value.assetClassValueOf (Ledger._decoratedTxOutValue o) (OnChain.tsToken ts)

        minusMinLovelace = 
           Ada.lovelaceValueOf (negate $ Ada.getLovelace Ledger.minAdaTxOut)
        
        lovelaceInScript =
          OnChain.lovelaces (Ledger._decoratedTxOutValue o P.<> minusMinLovelace)

      -- Min ada value in calculation
      if tokensInScript < n || lovelaceInScript < l
        then PlutusContract.logInfo @P.String "script doesn't contain enough assets"
        else do 
          let
            scriptOutValue =
              Ledger._decoratedTxOutValue o  P.<>
              Ada.lovelaceValueOf (negate l) P.<>
              Value.assetClassValue (OnChain.tsToken ts) (negate n)
          
            buyerReceiveValue =
              Ada.lovelaceValueOf l P.<>
              Value.assetClassValue (OnChain.tsToken ts) n
          
          let
            lookups =
              Constraints.unspentOutputs (Map.singleton oref o) P.<>
              Constraints.typedValidatorLookups (OnChain.tsTypedValidator ts)
             
            tx =
              Constraints.mustSpendScriptOutput oref
                (Ledger.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Withdraw n l) P.<>
              Constraints.mustPayToTheScriptWithInlineDatum p scriptOutValue      P.<>
              Constraints.mustPayToPubKey (OnChain.tsSeller ts) buyerReceiveValue P.<>
              Constraints.mustBeSignedBy (OnChain.tsSeller ts)
          
          -- Submit tx to buy tokens from the contract
          ledgerTx <- PlutusContract.submitTxConstraintsWith @OnChain.TS lookups tx
          void $ PlutusContract.awaitTxConfirmed (Ledger.getCardanoTxId ledgerTx)
          PlutusContract.logInfo @P.String $
            printf "withdrew %d tokens and %d lovelace" n l
          

-- ----------------------------------------------------------------------
-- Endpoints

type TSStartSchema =
        PlutusContract.Endpoint "start" OnChain.TokenSale --(Ledger.CurrencySymbol, Ledger.TokenName)

type TSUseSchema =
                           PlutusContract.Endpoint "set price"  Integer
        PlutusContract..\/ PlutusContract.Endpoint "add tokens" Integer
        PlutusContract..\/ PlutusContract.Endpoint "buy tokens" Integer
        PlutusContract..\/ PlutusContract.Endpoint "withdraw"   (Integer, Integer)
