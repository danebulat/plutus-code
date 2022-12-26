{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Spec.Trace where

import Control.Exception             (try)
import Control.Lens
import Control.Monad                 hiding (fmap)
import Control.Monad.Freer.Extras    as Extras
import Data.Default                  (Default (..))
import Data.IORef
import qualified Data.Map            as Map
import Data.Monoid                   (Last (..))
import Ledger
import qualified Ledger.Value        as Value
import Ledger.Ada                    as Ada
import PlutusTx.Coverage
import Plutus.Contract.Test
import Plutus.Contract.Test.Coverage 
import Plutus.Trace.Emulator         as Emulator
import qualified PlutusTx.Prelude    as Plutus
import System.Exit                   (ExitCode (..))
import Test.Tasty
import qualified Test.Tasty.HUnit    as HUnit
-- import           Plutus.Contract.Test.Coverage.ReportCoverage (writeCoverageReport)

import OffChain as OffChain
import OnChain  as OnChain

-- ----------------------------------------------------------------------
-- Test coverage

-- Doesn't work (output file empty)
testCoverage :: IO ()
testCoverage = do
  cref <- newCoverageRef
  e <- try $ defaultMain $ checkPredicateCoverageOptions
         myOptions
         "token sale trace"
         cref
         myPredicate5
         myTrace5
  case e of
    Left (c :: ExitCode) -> do
      putStrLn $ "Tasty exited with: " ++ show c
      report <- readCoverageRef cref
      writeCoverageReport "TokenSaleTrace" $ CoverageReport OnChain.tsCovIdx report
    Right () -> putStrLn "unexpected tasty result"


-- ----------------------------------------------------------------------
-- Tests function

tests :: TestTree
tests = testGroup "Token Sale Tests" [
  checkPredicateOptions myOptions "initialise" myPredicate1 myTrace1,
  checkPredicateOptions myOptions "set price"  myPredicate2 myTrace2,
  checkPredicateOptions myOptions "add tokens" myPredicate3 myTrace3,
  checkPredicateOptions myOptions "buy tokens" myPredicate4 myTrace4,
  checkPredicateOptions myOptions "withdraw"   myPredicate5 myTrace5
  ]

-- ----------------------------------------------------------------------
-- Emulator config

myOptions :: CheckOptions
myOptions = defaultCheckOptions & emulatorConfig .~ emCfg

emCfg :: Emulator.EmulatorConfig
emCfg = def {
  Emulator._initialChainState = Left $ Map.fromList
    [ (w1, v <> Value.assetClassValue (Value.AssetClass (tokenCurrency, threadTokenName)) 1
             <> Value.assetClassValue (Value.AssetClass (tokenCurrency, sellingTokenName)) 100)
    , (w2, v <> Value.assetClassValue (Value.AssetClass (tokenCurrency, sellingTokenName)) 100)
    ]
  }

-- Wallet starting lovelace value
v :: Value.Value
v = Ada.lovelaceValueOf 1_000_000_000

-- ----------------------------------------------------------------------
-- Helper functions

tokenCurrency :: Ledger.CurrencySymbol
tokenCurrency = "b89bbdd6b7e801b90fbd3a249f462fb049d43dd7a87d74a71cc97369"

threadTokenName :: Ledger.TokenName
threadTokenName = "STATE TOKEN"

sellingTokenName :: Ledger.TokenName
sellingTokenName = "TOK"

testTokenSale :: OnChain.TokenSale 
testTokenSale =
  OnChain.TokenSale {
    OnChain.tsSeller = mockWalletPaymentPubKeyHash w1,
    OnChain.tsToken  = Value.AssetClass (tokenCurrency, sellingTokenName),
    OnChain.tsTT     = Value.AssetClass (tokenCurrency, threadTokenName)
  }

sellingTokenCur :: AssetClass 
sellingTokenCur = Value.AssetClass (tokenCurrency, sellingTokenName)

threadTokenCur :: AssetClass 
threadTokenCur = Value.AssetClass (tokenCurrency, threadTokenName)


-- ----------------------------------------------------------------------
-- Traces and predicates

-- ----------------------------------------------------------------------
-- Scenario 1: Initialise token sale

myTrace1 :: EmulatorTrace ()
myTrace1 = do
  h <- activateContractWallet w1 OffChain.startEndpoint

  -- Initialise token sale
  Emulator.callEndpoint @"start" h testTokenSale
  void $ Emulator.waitNSlots 5
  
  Last m <- observableState h
  Extras.logInfo $ "initialised token sale: " ++ show m
  void $ Emulator.waitNSlots 1

myPredicate1 :: TracePredicate
myPredicate1 =
  walletFundsChange w1 (Value.assetClassValue adaCur (negate minLovelace)
                     <> Value.assetClassValue threadTokenCur (negate 1))
  where
    minLovelace = Ada.getLovelace Ledger.minAdaTxOut
    adaCur = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)


-- ----------------------------------------------------------------------
-- Scenario 2:  Initialise token sale and set token price

myTrace2 :: EmulatorTrace ()
myTrace2 = do
  h <- activateContractWallet w1 OffChain.startEndpoint
  
  -- Initialise token sale
  Emulator.callEndpoint @"start" h testTokenSale
  void $ Emulator.waitNSlots 2
  Last m <- observableState h

  case m of
    Nothing -> Extras.logError @String "error starting token sale"
    Just ts -> do
      Extras.logError @String $ "started token sale " ++ show ts
      
      -- Activate contract in wallet
      h1 <- activateContractWallet w1 $ OffChain.useEndpoints testTokenSale

      -- Call set price endpoint
      Emulator.callEndpoint @"set price" h1 1_000_000
      void $ Emulator.waitNSlots 2

myPredicate2 :: TracePredicate
myPredicate2 =
       walletFundsChange w1 valueAtW1
  .&&. valueAtAddress scriptAddr (== valueAtAddr)
  where
    minLovelace = Ada.getLovelace Ledger.minAdaTxOut
    adaCur      = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)
    scriptAddr  = OnChain.tsScriptAddress testTokenSale

    valueAtAddr = Value.assetClassValue adaCur minLovelace
               <> Value.assetClassValue threadTokenCur 1
               
    valueAtW1   = Value.assetClassValue adaCur (negate minLovelace)
               <> Value.assetClassValue threadTokenCur (negate 1)


-- ----------------------------------------------------------------------
-- Scenario 3: Add tokens to token sale

myTrace3 :: EmulatorTrace ()
myTrace3 = do
  h <- activateContractWallet w1 OffChain.startEndpoint
  
  -- Initialise token sale
  Emulator.callEndpoint @"start" h testTokenSale
  void $ Emulator.waitNSlots 2
  Last m <- observableState h

  case m of
    Nothing -> Extras.logError @String "error starting token sale"
    Just ts -> do
      Extras.logError @String $ "started token sale " ++ show ts
      
      h1 <- activateContractWallet w1 $ OffChain.useEndpoints testTokenSale
      h2 <- activateContractWallet w2 $ OffChain.useEndpoints testTokenSale

      -- Wallet 1 sets price
      Emulator.callEndpoint @"set price" h1 1_000_000
      void $ Emulator.waitNSlots 2

      -- Wallet 1 adds tokens
      Emulator.callEndpoint @"add tokens" h1 20
      void $ Emulator.waitNSlots 2

      -- Wallet 2 adds tokens
      Emulator.callEndpoint @"add tokens" h2 20
      void $ Emulator.waitNSlots 2

myPredicate3 :: TracePredicate
myPredicate3 =
       walletFundsChange w1 valueAtW1
  .&&. walletFundsChange w2 valueAtW2
  .&&. valueAtAddress scriptAddr (== valueAtAddr)
  where
    minLovelace = Ada.getLovelace Ledger.minAdaTxOut
    adaCur      = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)
    scriptAddr  = OnChain.tsScriptAddress testTokenSale

    valueAtAddr = Value.assetClassValue adaCur minLovelace
               <> Value.assetClassValue threadTokenCur 1
               <> Value.assetClassValue sellingTokenCur 40
               
    valueAtW1   = Value.assetClassValue adaCur (negate minLovelace)
               <> Value.assetClassValue threadTokenCur (negate 1)
               <> Value.assetClassValue sellingTokenCur (negate 20)

    valueAtW2   = Value.assetClassValue sellingTokenCur (negate 20)


-- ----------------------------------------------------------------------
-- Scenario 4: Buy tokens from token sale

myTrace4 :: EmulatorTrace ()
myTrace4 = do
  h <- activateContractWallet w1 OffChain.startEndpoint
  
  -- Initialise token sale
  Emulator.callEndpoint @"start" h testTokenSale
  void $ Emulator.waitNSlots 2
  Last m <- observableState h

  case m of
    Nothing -> Extras.logError @String "error starting token sale"
    Just ts -> do
      Extras.logError @String $ "started token sale " ++ show ts
      
      h1 <- activateContractWallet w1 $ OffChain.useEndpoints testTokenSale
      h2 <- activateContractWallet w2 $ OffChain.useEndpoints testTokenSale

      -- Wallet 1 sets price (Pass)
      Emulator.callEndpoint @"set price" h1 1_000_000
      void $ Emulator.waitNSlots 2

      -- Wallet 1 adds tokens (Pass)
      Emulator.callEndpoint @"add tokens" h1 20
      void $ Emulator.waitNSlots 2

      -- Wallet 2 buys 10 tokens for 10 ADA (Pass)
      Emulator.callEndpoint @"buy tokens" h2 10
      void $ Emulator.waitNSlots 2

      -- Wallet 2 buys 500 tokens for 500 ADA (Fail)
      Emulator.callEndpoint @"buy tokens" h2 500
      void $ Emulator.waitNSlots 2

myPredicate4 :: TracePredicate
myPredicate4 =
       walletFundsChange w1 valueAtW1
  .&&. walletFundsChange w2 valueAtW2
  .&&. valueAtAddress scriptAddr (== valueAtAddr)
  where
    minLovelace = Ada.getLovelace Ledger.minAdaTxOut
    adaCur      = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)
    scriptAddr  = OnChain.tsScriptAddress testTokenSale

    valueAtAddr = Value.assetClassValue adaCur          minLovelace
               <> Value.assetClassValue adaCur          10_000_000
               <> Value.assetClassValue threadTokenCur  1
               <> Value.assetClassValue sellingTokenCur 10
               
    valueAtW1   = Value.assetClassValue adaCur          (negate minLovelace)
               <> Value.assetClassValue threadTokenCur  (negate 1)
               <> Value.assetClassValue sellingTokenCur (negate 20)

    valueAtW2   = Value.assetClassValue sellingTokenCur 10
               <> Value.assetClassValue adaCur          (negate 10_000_000)


-- ----------------------------------------------------------------------
-- Scenario 5: Withdraw tokens from token sale

myTrace5 :: EmulatorTrace ()
myTrace5 = do
  h <- activateContractWallet w1 OffChain.startEndpoint
  
  -- Initialise token sale
  Emulator.callEndpoint @"start" h testTokenSale
  void $ Emulator.waitNSlots 2
  Last m <- observableState h

  case m of
    Nothing -> Extras.logError @String "error starting token sale"
    Just ts -> do
      Extras.logError @String $ "started token sale " ++ show ts
      
      h1 <- activateContractWallet w1 $ OffChain.useEndpoints testTokenSale
      h2 <- activateContractWallet w2 $ OffChain.useEndpoints testTokenSale

      -- Wallet 1 sets price (Pass)
      Emulator.callEndpoint @"set price" h1 1_000_000
      void $ Emulator.waitNSlots 2

      -- Wallet 1 adds 80 tokens (Pass)
      Emulator.callEndpoint @"add tokens" h1 80
      void $ Emulator.waitNSlots 2

      -- Wallet 2 buys 70 tokens for 70 ADA (Pass)
      Emulator.callEndpoint @"buy tokens" h2 70
      void $ Emulator.waitNSlots 2

      -- Wallet 1 withdraws 60 ADA and 2 tokens from contract (Pass)
      Emulator.callEndpoint @"withdraw" h1 (2, 60_000_000)
      void $ Emulator.waitNSlots 2

myPredicate5 :: TracePredicate
myPredicate5 =
       walletFundsChange w1 valueAtW1
  .&&. walletFundsChange w2 valueAtW2
  .&&. valueAtAddress scriptAddr (== valueAtAddr)
  where
    minLovelace = Ada.getLovelace Ledger.minAdaTxOut
    adaCur      = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)
    scriptAddr  = OnChain.tsScriptAddress testTokenSale

    valueAtAddr = Value.assetClassValue adaCur          minLovelace
               <> Value.assetClassValue adaCur          10_000_000
               <> Value.assetClassValue threadTokenCur  1
               <> Value.assetClassValue sellingTokenCur 8
               
    valueAtW1   = Value.assetClassValue adaCur          (negate minLovelace)
               <> Value.assetClassValue adaCur          60_000_000
               <> Value.assetClassValue threadTokenCur  (negate 1)
               <> Value.assetClassValue sellingTokenCur (negate 78)

    valueAtW2   = Value.assetClassValue sellingTokenCur 70
               <> Value.assetClassValue adaCur          (negate 70_000_000)
