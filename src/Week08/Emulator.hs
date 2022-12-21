{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Week08.Emulator where


import qualified Plutus.Trace.Emulator     as Emulator
import Control.Monad.Freer.Extras          as Extras
import Data.Default                        (Default (..))
import Data.Functor                        (void)
import Data.Monoid                         (Last(..))
import qualified Data.Map                  as Map
import Prelude                             (IO, Show(..), undefined, String)

import PlutusTx.Prelude
import Plutus.Trace
import Wallet.Emulator.Wallet              (knownWallet, mockWalletPaymentPubKeyHash, Wallet(..))
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Ledger.Ada                as Ada
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Ledger.TimeSlot           as TimeSlot

import qualified Week08.OnChain            as OnChain
import qualified Week08.OffChain           as OffChain


-- ----------------------------------------------------------------------
-- Main test

test :: IO ()
test = test6


-- ----------------------------------------------------------------------
-- Helper functions

tokenCurrency :: LedgerApiV2.CurrencySymbol
tokenCurrency = "b89bbdd6b7e801b90fbd3a249f462fb049d43dd7a87d74a71cc97369"

threadTokenName :: LedgerApiV2.TokenName
threadTokenName = "STATE TOKEN"

sellingTokenName :: LedgerApiV2.TokenName
sellingTokenName = "TOK"

testTokenSale :: OnChain.TokenSale 
testTokenSale =
  OnChain.TokenSale {
    OnChain.tsSeller = mockWalletPaymentPubKeyHash w1,
    OnChain.tsToken  = Value.AssetClass (tokenCurrency, sellingTokenName),
    OnChain.tsTT     = Value.AssetClass (tokenCurrency, threadTokenName)
  }

w1, w2 :: Wallet
w1 = knownWallet 1
w2 = knownWallet 2


-- ----------------------------------------------------------------------
-- Tests

-- Test to initialise the contract
test1 :: IO ()
test1 = runEmulatorTraceIO' def emCfg myTrace1

-- Tests to set the token price
test2 :: IO ()
test2 = runEmulatorTraceIO' def emCfg myTrace2

test3 :: IO ()
test3 = runEmulatorTraceIO' def emCfg myTrace3

-- Test to add tokens to script
test4 :: IO ()
test4 = runEmulatorTraceIO' def emCfg2 myTrace4

-- Test to buy tokens from script
test5 :: IO ()
test5 = runEmulatorTraceIO' def emCfg myTrace5

-- Test to withdraw tokens from script
test6 :: IO ()
test6 = runEmulatorTraceIO' def emCfg myTrace6


-- ----------------------------------------------------------------------
-- Configuration setup

-- Wallet 1 starts with the NFT + 1000 ADA
emCfg :: Emulator.EmulatorConfig
emCfg = def {
  Emulator._initialChainState = Left $ Map.fromList
    [ (w1, v <> Value.assetClassValue (Value.AssetClass (tokenCurrency, threadTokenName)) 1
             <> Value.assetClassValue (Value.AssetClass (tokenCurrency, sellingTokenName)) 100)
    , (w2, v)
    ]
  }

-- Wallet 1 and 2 both start with some TOK tokens
emCfg2 :: Emulator.EmulatorConfig
emCfg2 = def {
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
-- Emulator trace 

-- Trace 6: Seller withdraws tokens and ADA from toke sale
myTrace6 :: EmulatorTrace ()
myTrace6 = do
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


-- Trace 5: Buy tokens from token sale
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

      -- Wallet 1 adds tokens (Pass)
      Emulator.callEndpoint @"add tokens" h1 20
      void $ Emulator.waitNSlots 2

      -- Wallet 2 buys 10 tokens for 10 ADA (Pass)
      Emulator.callEndpoint @"buy tokens" h2 10
      void $ Emulator.waitNSlots 2

      -- Wallet 2 buys 500 tokens for 500 ADA (Fail)
      Emulator.callEndpoint @"buy tokens" h2 500
      void $ Emulator.waitNSlots 2


-- Trace 4: Add tokens to token sale
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

      -- Wallet 2 adds tokens (Pass)
      Emulator.callEndpoint @"add tokens" h2 20
      void $ Emulator.waitNSlots 2
      

-- Trace 3: Initialise token sale and set token price twice
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

     -- Wallet 1 sets price (Pass)
     Emulator.callEndpoint @"set price" h1 1_000_000
     void $ Emulator.waitNSlots 2

     -- Wallet 2 sets price (Fail)
     Emulator.callEndpoint @"set price" h2 700_000
     void $ Emulator.waitNSlots 2

     -- Wallet 1 sets price (Pass)
     Emulator.callEndpoint @"set price" h1 500_000
     void $ Emulator.waitNSlots 2


-- Trace 2: Initialise token sale and set token price
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


-- Trace 1: Initialise token sale
myTrace1 :: EmulatorTrace ()
myTrace1 = do
  h <- activateContractWallet w1 OffChain.startEndpoint

  -- Initialise token sale
  Emulator.callEndpoint @"start" h testTokenSale
  
  void $ Emulator.waitNSlots 5
  
  Last m <- observableState h
  Extras.logInfo $ "initialised token sale: " ++ show m
  
  void $ Emulator.waitNSlots 1
  
