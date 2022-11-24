{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module NFT01.Emulator where

import qualified Plutus.Trace.Emulator  as Emulator
import Data.Default                     (Default (..))
import Control.Monad.Freer.Extras       as Extras
import Data.Functor                     (void)
import Plutus.Trace                     
import Wallet.Emulator.Wallet           (knownWallet, mockWalletPaymentPubKeyHash)
import qualified Wallet.Emulator.Wallet as Wallet
import qualified Ledger.TimeSlot        as TimeSlot
import qualified NFT01.OffChain         as OffChain


test :: IO ()
test = Emulator.runEmulatorTraceIO trace1

trace1 :: Emulator.EmulatorTrace ()
trace1 = do
  let tn = "ABC"
      w1 = knownWallet 1
      w2 = knownWallet 2

  -- active endpoints on wallets
  h1 <- activateContractWallet w1 OffChain.endpoints
  h2 <- activateContractWallet w2 OffChain.endpoints

  -- mockWalletAddress returns the wallet address 
  callEndpoint @"mint" h1 $ OffChain.NFTParams
    { OffChain.npToken   = tn
    , OffChain.npAddress = Wallet.mockWalletAddress w1
    }
  void $ Emulator.waitNSlots 2
  
  callEndpoint @"mint" h2 $ OffChain.NFTParams
    { OffChain.npToken   = tn
    , OffChain.npAddress = Wallet.mockWalletAddress w2
    }
  void $ Emulator.waitNSlots 2

