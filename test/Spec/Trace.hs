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
import Plutus.Contract.Test
import Plutus.Contract.Test.Coverage 
import Plutus.Trace.Emulator         as Emulator
import qualified PlutusTx.Prelude    as Plutus
import System.Exit                   (ExitCode (..))
import Test.Tasty
import qualified Test.Tasty.HUnit    as HUnit
-- import           Plutus.Contract.Test.Coverage.ReportCoverage (writeCoverageReport)

import Week08.OffChain as OffChain
import Week08.OnChain  as OnChain
import qualified Ledger.Ada as Ledger


-- ----------------------------------------------------------------------
-- Tests function

tests :: TestTree
tests = checkPredicateOptions
    myOptions
    "token sale trace"
    myPredicate
    myTrace1

-- ----------------------------------------------------------------------
-- Emulator config

myOptions :: CheckOptions
myOptions = defaultCheckOptions & emulatorConfig .~ emCfg

emCfg :: Emulator.EmulatorConfig
emCfg = def {
  Emulator._initialChainState = Left $ Map.fromList
    [ (w1, v <> Value.assetClassValue (Value.AssetClass (tokenCurrency, threadTokenName)) 1
             <> Value.assetClassValue (Value.AssetClass (tokenCurrency, sellingTokenName)) 100)
    , (w2, v)
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

myPredicate :: TracePredicate
myPredicate =
  walletFundsChange w1 (Value.assetClassValue adaCur (negate minAsInt)
                     <> Value.assetClassValue threadTokenCur (negate 1))
  where
    minAsInt = Ledger.getLovelace $ Ledger.minAdaTxOut
    adaCur = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)
                          
