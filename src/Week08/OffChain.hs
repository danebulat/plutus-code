{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DerivingStrategies  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Week08.OffChain where


import Control.Monad                       (void, forever)
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
import qualified Plutus.Contract           as PC
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
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Constraints as Constraints


-- ----------------------------------------------------------------------
-- Helper Functions

findTsOutput
    :: OnChain.TokenSale
    -> PC.Contract w s Text
         (Maybe (Ledger.TxOutRef, Ledger.DecoratedTxOut, Integer))
findTsOutput ts = do
  utxos <- PC.utxosAt $ OnChain.tsScriptAddress ts

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

startTS :: OnChain.TokenSale -> PC.Contract (Last OnChain.TokenSale) s Text ()
startTS ts = do
  pkh <- PC.ownFirstPaymentPubKeyHash
  
  let
    v       = Value.assetClassValue (OnChain.tsTT ts) 1 P.<>
              Ada.lovelaceValueOf (Ada.getLovelace Ledger.minAdaTxOut)
    dat     = 0
    
    lookups = Constraints.typedValidatorLookups (OnChain.tsTypedValidator ts)
    tx      = Constraints.mustPayToTheScriptWithInlineDatum dat v P.<>
              Constraints.mustBeSignedBy (OnChain.tsSeller ts)

    ts'     = OnChain.TokenSale pkh (OnChain.tsToken ts) (OnChain.tsTT ts)

  -- Submit tx to produce initial UTXO
  ledgerTx <- PC.submitTxConstraintsWith @OnChain.TS lookups tx
  void $ PC.awaitTxConfirmed (Ledger.getCardanoTxId ledgerTx)

  -- Tell token sale
  PC.tell $ Last $ Just ts'
  PC.logInfo @P.String "produced token sale output"


-- ----------------------------------------------------------------------
-- Action: Set Price

setPrice :: OnChain.TokenSale -> Integer -> PC.Contract w s Text ()
setPrice ts p = do
  m <- findTsOutput ts
  case m of
    Nothing -> PC.logInfo @P.String "token sale utxo not found"
    Just (oref, o, _) -> do
      PC.logInfo @P.String "token sale utxo found"

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
      ledgerTx <- PC.submitTxConstraintsWith @OnChain.TS lookups tx
      void $ PC.awaitTxConfirmed (Ledger.getCardanoTxId ledgerTx)
      PC.logInfo @P.String $ printf "set token price to %d lovelace" p

      -- DEBUGGING ----------------------------------------------------
      -- Get UTXO back and log datum value.

      m' <- findTsOutput ts
      case m' of
        Nothing        -> PC.logInfo @P.String "token sale utxo not found"
        Just (_, _, d) -> PC.logInfo @P.String $ printf "datum set to: %d" d


-- ----------------------------------------------------------------------
-- Action: Add Tokens

addTokens :: OnChain.TokenSale -> Integer -> PC.Contract w s Text ()
addTokens ts n = do
  m <- findTsOutput ts
  case m of
    Nothing -> PC.logInfo @P.String "token sale utxo not found"
    Just (oref, o, p) -> do
      PC.logInfo @P.String "token sale utxo found"

      pkh <- PC.ownFirstPaymentPubKeyHash

      -- TODO: Check if script has minimum ADA. Add to tx if required.
      
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
      ledgerTx <- PC.submitTxConstraintsWith @OnChain.TS lookups tx
      void $ PC.awaitTxConfirmed (Ledger.getCardanoTxId ledgerTx)
      PC.logInfo @P.String $ printf "paid %d tokens to the script" n


-- ----------------------------------------------------------------------
-- Action: Buy Tokens

buyTokens :: OnChain.TokenSale -> Integer -> PC.Contract w s Text ()
buyTokens ts n = do
  m <- findTsOutput ts
  case m of
    Nothing -> PC.logInfo @P.String "token sale utxo not found"
    Just (oref, o, p) -> do
      PC.logInfo @P.String "token sale utxo found"

      -- Check if contract holds enough tokens
      if Value.assetClassValueOf (Ledger._decoratedTxOutValue o) (OnChain.tsToken ts) < n
        then PC.logInfo @P.String "contract doesn't hold enough tokens"
        else do
          pkh <- PC.ownFirstPaymentPubKeyHash
          
          let
            scriptOutVal =
              Ledger._decoratedTxOutValue o P.<>
              Ada.lovelaceValueOf (p * n)   P.<>
              Value.assetClassValue (OnChain.tsToken ts) (negate n)

            buyerReceiveValue =
              Value.assetClassValue (OnChain.tsToken ts) n P.<>
              Ada.adaValueOf (Ada.getAda Ledger.minAdaTxOut) -- add min ada
          
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
          ledgerTx <- PC.submitTxConstraintsWith @OnChain.TS lookups tx
          void $ PC.awaitTxConfirmed (Ledger.getCardanoTxId ledgerTx)
          PC.logInfo @P.String $
            printf "bought %d tokens for %d lovelace" n (n * p)


-- ----------------------------------------------------------------------
-- Action: Withdraw Tokens

withdraw :: OnChain.TokenSale -> Integer -> Integer -> PC.Contract w s Text ()
withdraw ts n l = do
  m <- findTsOutput ts
  case m of
    Nothing -> PC.logInfo @P.String "token sale utxo not found"
    Just (oref, o, p) -> do
      PC.logInfo @P.String "token sale utxo found"

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
        then PC.logInfo @P.String "script doesn't contain enough assets"
        else do 
          let
            scriptOutValue =
              Ledger._decoratedTxOutValue o  P.<>
              Ada.lovelaceValueOf (negate l) P.<>
              Value.assetClassValue (OnChain.tsToken ts) (negate n)
          
            sellerReceiveValue =
              Ada.lovelaceValueOf l P.<>
              Value.assetClassValue (OnChain.tsToken ts) n  P.<>
              Ada.adaValueOf (Ada.getAda Ledger.minAdaTxOut) -- add min ada
          
          let
            lookups =
              Constraints.unspentOutputs (Map.singleton oref o) P.<>
              Constraints.typedValidatorLookups (OnChain.tsTypedValidator ts)
             
            tx =
              Constraints.mustSpendScriptOutput oref
                (Ledger.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Withdraw n l)  P.<>
              Constraints.mustPayToTheScriptWithInlineDatum p scriptOutValue       P.<>
              Constraints.mustPayToPubKey (OnChain.tsSeller ts) sellerReceiveValue P.<>
              Constraints.mustBeSignedBy (OnChain.tsSeller ts)
          
          -- Submit tx to buy tokens from the contract
          ledgerTx <- PC.submitTxConstraintsWith @OnChain.TS lookups tx
          void $ PC.awaitTxConfirmed (Ledger.getCardanoTxId ledgerTx)
          PC.logInfo @P.String $
            printf "withdrew %d tokens and %d lovelace" n l
          

-- ----------------------------------------------------------------------
-- Schema

type TSStartSchema =
        PC.Endpoint "start" OnChain.TokenSale -- (Ledger.CurrencySymbol, Ledger.TokenName)

type TSUseSchema =
               PC.Endpoint "set price"  Integer
        PC..\/ PC.Endpoint "add tokens" Integer
        PC..\/ PC.Endpoint "buy tokens" Integer
        PC..\/ PC.Endpoint "withdraw"   (Integer, Integer)


-- ----------------------------------------------------------------------
-- Endpoints

startEndpoint :: PC.Contract (Last OnChain.TokenSale) TSStartSchema Text ()
startEndpoint = forever
              $ PC.handleError PC.logError
              $ PC.awaitPromise
              $ PC.endpoint @"start" startTS


useEndpoints :: OnChain.TokenSale -> PC.Contract () TSUseSchema Text ()
useEndpoints = useEndpoints'


useEndpoints'
    :: ( PC.HasEndpoint "set price"  Integer s
       , PC.HasEndpoint "add tokens" Integer s
       , PC.HasEndpoint "buy tokens" Integer s
       , PC.HasEndpoint "withdraw"   (Integer, Integer) s
       )
    => OnChain.TokenSale
    -> PC.Contract () s Text ()
useEndpoints' ts = forever
                $ PC.handleError PC.logError
                $ PC.awaitPromise
                $ setPrice'  `PC.select`
                  addTokens' `PC.select`
                  buyTokens' `PC.select`
                  withdraw'
  where
    setPrice'  = PC.endpoint @"set price"  $ setPrice  ts
    addTokens' = PC.endpoint @"add tokens" $ addTokens ts
    buyTokens' = PC.endpoint @"buy tokens" $ buyTokens ts
    withdraw'  = PC.endpoint @"withdraw"   $ P.uncurry $ withdraw ts
