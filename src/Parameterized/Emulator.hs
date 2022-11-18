{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Parameterized.Emulator where

import qualified Plutus.Trace.Emulator  as Emulator
import Data.Default                     (Default (..))
import Control.Monad.Freer.Extras       as Extras
import Data.Functor                     (void)
import Plutus.Trace
import Wallet.Emulator.Wallet           (knownWallet, mockWalletPaymentPubKeyHash)

import qualified Ledger.TimeSlot        as TimeSlot

import qualified Parameterized.OffChain as OffChain


test :: IO ()
test = Emulator.runEmulatorTraceIO trace1 


trace1 :: Emulator.EmulatorTrace ()
trace1 = do
  h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.endpoints
  h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.endpoints
  h3 <- Emulator.activateContractWallet (knownWallet 3) OffChain.endpoints

  Emulator.callEndpoint @"start" h1 $ OffChain.StartParams {
      OffChain.spCreator     = mockWalletPaymentPubKeyHash $ knownWallet 3,
      OffChain.spBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2,
      OffChain.spDeadline    = TimeSlot.slotToBeginPOSIXTime def 30,
      OffChain.spGuess       = 20,
      OffChain.spAmount      = 40000000
  }
  void $ waitNSlots 2

  Emulator.callEndpoint @"start" h3 $ OffChain.StartParams {
      OffChain.spCreator     = mockWalletPaymentPubKeyHash $ knownWallet 3,
      OffChain.spBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 1,
      OffChain.spDeadline    = TimeSlot.slotToBeginPOSIXTime def 15,
      OffChain.spGuess       = 1,
      OffChain.spAmount      = 40000000
  }
  void $ waitNSlots 20

  Emulator.callEndpoint @"grab" h1 $ OffChain.GrabParams {
        OffChain.gpCreator  = mockWalletPaymentPubKeyHash $ knownWallet 3
      , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 15
      , OffChain.gpGuess    = 1
  }
  void $ waitNSlots 15

  Emulator.callEndpoint @"grab" h2 $ OffChain.GrabParams {
        OffChain.gpCreator  = mockWalletPaymentPubKeyHash $ knownWallet 3
      , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 30
      , OffChain.gpGuess    = 20
  }
  s <- waitNSlots 2
  
  Extras.logInfo $ "reached " ++ show s

