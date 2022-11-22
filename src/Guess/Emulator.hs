{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NumericUnderscores #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Guess.Emulator where

import qualified Plutus.Trace.Emulator     as Emulator
import Control.Monad.Freer.Extras          as Extras
import Data.Default                        (Default (..))
import Data.Functor                        (void)
import Plutus.Trace
import Wallet.Emulator.Wallet              (knownWallet, mockWalletPaymentPubKeyHash)
import qualified Ledger.TimeSlot           as TimeSlot
import qualified Guess.OffChain            as OffChain


test :: IO ()
test = Emulator.runEmulatorTraceIO trace1

trace1 :: Emulator.EmulatorTrace ()
trace1 = do
  h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.endpoints
  h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.endpoints

  Emulator.callEndpoint @"give" h1 $ OffChain.GiveParams {
    OffChain.giveAmount = 40_000_000,
    OffChain.giveDat    = 20
  }

  void $ waitNSlots 2
  Emulator.callEndpoint @"grab" h2 $ OffChain.GrabParams {
    OffChain.grabRedeem = 21
  }

  void $ waitNSlots 2
  Emulator.callEndpoint @"grab" h2 $ OffChain.GrabParams {
    OffChain.grabRedeem = 20
  }

  s <- waitNSlots 2
  Extras.logInfo $ "reached " ++ show s
