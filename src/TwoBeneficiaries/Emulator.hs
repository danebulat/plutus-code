{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NumericUnderscores #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module TwoBeneficiaries.Emulator where 

import qualified Plutus.Trace.Emulator     as Emulator
import Data.Default                        (Default (..))
import Control.Monad.Freer.Extras          as Extras
import Data.Functor                        (void)
import Plutus.Trace                        
import Wallet.Emulator.Wallet              (knownWallet, mockWalletPaymentPubKeyHash)
import qualified Ledger.TimeSlot           as TimeSlot

import qualified TwoBeneficiaries.OffChain as OffChain


test :: IO ()
test = Emulator.runEmulatorTraceIO trace1

trace1 :: Emulator.EmulatorTrace ()
trace1 = do
  h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.endpoints
  h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.endpoints

  -- beneficiary1 = wallet 1
  -- beneficiary2 = wallet 2
  -- deadline     = slot 10
  
  Emulator.callEndpoint @"give" h1 $ OffChain.GiveParams {
    OffChain.gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2,
    OffChain.gpDeadline    = {-1671501481000,-}  TimeSlot.slotToEndPOSIXTime def 20,
    OffChain.gpAmount      = 20_000_000
  }
  
  void $ waitNSlots 10

  Emulator.callEndpoint @"grab" h1 ()

  void $ waitNSlots 2


