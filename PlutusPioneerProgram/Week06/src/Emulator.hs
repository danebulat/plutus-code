{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Emulator where

import qualified Plutus.Trace.Emulator     as Emulator
import qualified Plutus.Contract           as PlutusContract
import Data.Default                        (Default (..))
import Control.Monad.Freer.Extras          as Extras
import Data.Functor                        (void)
import Plutus.Trace                        
import Wallet.Emulator.Wallet              (knownWallet, mockWalletAddress)
import qualified Ledger.TimeSlot           as TimeSlot
import qualified OffChain                  as OffChain

-- -------------------------------------------------------------------
-- Run `testToken` in repl
-- -------------------------------------------------------------------

testToken :: IO ()
testToken = Emulator.runEmulatorTraceIO tokenTrace

tokenTrace :: Emulator.EmulatorTrace ()
tokenTrace = do
    let w1 = knownWallet 1

    -- Specify mintToken contract and two specific types for
    -- the 'w' and 's' schema parts.
    
    void $ activateContractWallet w1 $
             OffChain.mintToken @() @PlutusContract.Empty OffChain.TokenParams {
      OffChain.tpToken   = "USDT",
      OffChain.tpAmount  = 100_000,
      OffChain.tpAddress = mockWalletAddress w1
    }

    void $ Emulator.waitNSlots 2
