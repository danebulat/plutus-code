{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Emulator where

import qualified Plutus.Trace.Emulator     as Emulator
import Data.Default                        (Default (..))
import Control.Monad.Freer.Extras          as Extras
import Data.Functor                        (void)
import Plutus.Trace                        
import Wallet.Emulator.Wallet              (knownWallet, mockWalletPaymentPubKeyHash)
import qualified Ledger.TimeSlot           as TimeSlot
import qualified OffChain

test :: IO ()
test = Emulator.runEmulatorTraceIO trace1

trace1 :: Emulator.EmulatorTrace ()
trace1 = do
  -- native asset token name 
  let tn = "ABC"

  -- activate wallets
  h1 <- activateContractWallet (knownWallet 1) OffChain.endpoints
  h2 <- activateContractWallet (knownWallet 2) OffChain.endpoints

  -- mint 555 tokens
  Emulator.callEndpoint @"mint" h1 $ OffChain.MintParams {
    OffChain.mpTokenName = tn,
    OffChain.mpAmount    = 555
  }
  void $ Emulator.waitNSlots 2

  -- mint 444 tokens
  Emulator.callEndpoint @"mint" h2 $ OffChain.MintParams {
    OffChain.mpTokenName = tn,
    OffChain.mpAmount    = 444
  }
  void $ Emulator.waitNSlots 2

  -- burn 222 tokens
  Emulator.callEndpoint @"mint" h1 $ OffChain.MintParams {
    OffChain.mpTokenName = tn,
    OffChain.mpAmount    = -222
  }
  void $ Emulator.waitNSlots 2
