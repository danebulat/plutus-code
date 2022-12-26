{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Parameterized.Emulator where

import qualified Plutus.Trace.Emulator  as Emulator
import qualified Ledger.TimeSlot        as TimeSlot
import Data.Default                     (Default (..))
import Control.Monad.Freer.Extras       as Extras
import Data.Functor                     (void)
import Plutus.Trace
import Wallet.Emulator.Wallet           (knownWallet, mockWalletPaymentPubKeyHash)
import qualified Parameterized.OffChain as OffChain


test :: IO ()
test = Emulator.runEmulatorTraceIO trace1 


trace1 :: Emulator.EmulatorTrace ()
trace1 = do
  h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.endpoints
  h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.endpoints
  h3 <- Emulator.activateContractWallet (knownWallet 3) OffChain.endpoints

  -- Pay 40 ADA
  -- - royalties to wallet 3
  -- - to wallet 2
  -- - after slot 30
  -- - 10% to wallet 1
  -- - redeemer must be 20
  Emulator.callEndpoint @"start" h1 $ OffChain.StartParams {
      OffChain.spCreator     = mockWalletPaymentPubKeyHash $ knownWallet 3,
      OffChain.spBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2,
      OffChain.spDeadline    = TimeSlot.slotToBeginPOSIXTime def 30,
      OffChain.spGuess       = 20,
      OffChain.spAmount      = 40000000
  }
  void $ waitNSlots 2

  -- Pay 40 ADA
  -- - royalties to wallet 1
  -- - to wallet 1
  -- - after slot 15
  -- - 10% to wallet 3
  -- - redeemer must be 1
  Emulator.callEndpoint @"start" h3 $ OffChain.StartParams {
      OffChain.spCreator     = mockWalletPaymentPubKeyHash $ knownWallet 3,
      OffChain.spBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 1,
      OffChain.spDeadline    = TimeSlot.slotToBeginPOSIXTime def 15,
      OffChain.spGuess       = 1,
      OffChain.spAmount      = 40000000
  }

  void $ waitUntilSlot 22

  Emulator.callEndpoint @"grab" h1 $ OffChain.GrabParams {
        OffChain.gpCreator  = mockWalletPaymentPubKeyHash $ knownWallet 3
      , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 15
      , OffChain.gpGuess    = 1
  }

  void $ waitUntilSlot 35

  Emulator.callEndpoint @"grab" h2 $ OffChain.GrabParams {
      OffChain.gpCreator  = mockWalletPaymentPubKeyHash $ knownWallet 3,
      OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
      OffChain.gpGuess    = 20
  }

  s <- waitNSlots 2
  Extras.logInfo $ "reached " ++ show s

