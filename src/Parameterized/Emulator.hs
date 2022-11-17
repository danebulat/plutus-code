{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Parameterized.Emulator where

import qualified Plutus.Trace.Emulator  as Emulator
import Data.Default                     (Default (..))
import Control.Monad.Freer.Extras       as Extras
import Data.Functor                     (void)
import Plutus.Trace
import qualified Wallet.Emulator.Wallet as Wallet
import qualified Ledger.TimeSlot        as TimeSlot

import qualified Parameterized.OffChain as OffChain

test :: IO ()
test = Emulator.runEmulatorTraceIO trace1 


trace1 :: Emulator.EmulatorTrace ()
trace1 = do
  h1 <- Emulator.activateContractWallet (Wallet.knownWallet 1) OffChain.endpoints
  h2 <- Emulator.activateContractWallet (Wallet.knownWallet 2) OffChain.endpoints
  h3 <- Emulator.activateContractWallet (Wallet.knownWallet 3) OffChain.endpoints

  Emulator.callEndpoint @"start" h1 $ OffChain.StartParams {
        OffChain.spCreator = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3,
        OffChain.spBeneficiary = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2,
        OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
        OffChain.spGuess = 20,
        OffChain.spAmount = 40000000
    }
  void $ waitNSlots 2

  Emulator.callEndpoint @"start" h3 $ OffChain.StartParams {
      OffChain.spCreator = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3,
      OffChain.spBeneficiary = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 1,
      OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 15,
      OffChain.spGuess = 1,
      OffChain.spAmount = 40000000
  }
  void $ waitNSlots 20

  Emulator.callEndpoint @"grab" h1 $ OffChain.GrabParams {
        OffChain.gpCreator  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3
      , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 15
      , OffChain.gpGuess    = 1
  }
  void $ waitNSlots 15

  Emulator.callEndpoint @"grab" h2 $ OffChain.GrabParams {
        OffChain.gpCreator  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3
      , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 30
      , OffChain.gpGuess    = 20
  }
  s <- waitNSlots 2
  
  Extras.logInfo $ "reached " ++ show s

