{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module MintBurn.Emulator where 

import Control.Monad                    (void)
import qualified Wallet.Emulator.Wallet as Wallet 
import qualified Plutus.Trace.Emulator  as Emulator
import qualified MintBurn.OffChain      as OffChain
import PlutusTx.Prelude
import qualified Prelude as P

test :: P.IO ()
test = Emulator.runEmulatorTraceIO $ do 
  let tn = "Test"
  h1 <- Emulator.activateContractWallet (Wallet.knownWallet 1) OffChain.endpoints
  h2 <- Emulator.activateContractWallet (Wallet.knownWallet 2) OffChain.endpoints
  h3 <- Emulator.activateContractWallet (Wallet.knownWallet 3) OffChain.endpoints

  -- Wallet 1 mints token
  Emulator.callEndpoint @"mint" h1 $ OffChain.MintParams 
    {
      OffChain.mpAddress   = Wallet.mockWalletAddress (Wallet.knownWallet 1),
      OffChain.mpTokenName = tn,
      OffChain.mpQuantity  = 1,
      OffChain.mpAction    = True, -- mint
      OffChain.mpUtxo      = Nothing
    }
  void $ Emulator.waitNSlots 5

  -- Wallet 2 mints token
  Emulator.callEndpoint @"mint" h2 $ OffChain.MintParams 
    {
      OffChain.mpAddress   = Wallet.mockWalletAddress (Wallet.knownWallet 2),
      OffChain.mpTokenName = tn,
      OffChain.mpQuantity  = 1,
      OffChain.mpAction    = True, -- mint 
      OffChain.mpUtxo      = Nothing
    }
  void $ Emulator.waitNSlots 5

  -- Wallet 3 mints token
  Emulator.callEndpoint @"mint" h3 $ OffChain.MintParams 
    {
      OffChain.mpAddress   = Wallet.mockWalletAddress (Wallet.knownWallet 3),
      OffChain.mpTokenName = tn,
      OffChain.mpQuantity  = 1,
      OffChain.mpAction    = True, -- mint
      OffChain.mpUtxo      = Nothing
    }
  void $ Emulator.waitNSlots 5

  w3Utxo <- Emulator.observableState h3

  -- Wallet 3 burns token 
  Emulator.callEndpoint @"mint" h3 $ OffChain.MintParams 
    {
      OffChain.mpAddress   = Wallet.mockWalletAddress (Wallet.knownWallet 3),
      OffChain.mpTokenName = tn,
      OffChain.mpQuantity  = -1,
      OffChain.mpAction    = False, -- burn
      OffChain.mpUtxo      = Just $ head w3Utxo 
    }

  void $ Emulator.waitNSlots 5

