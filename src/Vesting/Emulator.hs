{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NumericUnderscores #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Vesting.Emulator where


import qualified Plutus.Trace.Emulator     as Emulator
import Data.Default                        (Default (..))
import Control.Monad.Freer.Extras          as Extras
import Data.Functor                        (void)
import Plutus.Trace                        
import Wallet.Emulator.Wallet              (knownWallet, mockWalletPaymentPubKeyHash)
import qualified Ledger.TimeSlot           as TimeSlot

import qualified Vesting.OffChain          as OffChain

test :: IO ()
test = Emulator.runEmulatorTraceIO trace1

trace1 :: Emulator.EmulatorTrace ()
trace1 = do
  h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.endpoints
  h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.endpoints
  h3 <- Emulator.activateContractWallet (knownWallet 3) OffChain.endpoints

  Emulator.callEndpoint @"give" h1 $ OffChain.GiveParams {
    OffChain.gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2,
    OffChain.gpDeadline    = TimeSlot.slotToEndPOSIXTime def 20,
    OffChain.gpAmount      = 20_000_000
  }

  void $ waitNSlots 1

  Emulator.callEndpoint @"give" h1 $ OffChain.GiveParams {
    OffChain.gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 3,
    OffChain.gpDeadline    = TimeSlot.slotToEndPOSIXTime def 10,
    OffChain.gpAmount      = 30_000_000
  }

  void $ waitNSlots 11

  Emulator.callEndpoint @"grab" h3 ()

  void $ waitNSlots 20

  Emulator.callEndpoint @"grab" h2 ()

  void $ waitNSlots 2
