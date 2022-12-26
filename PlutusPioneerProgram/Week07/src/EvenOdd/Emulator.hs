{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}

module EvenOdd.Emulator where

import qualified Plutus.Trace.Emulator     as Emulator
import Control.Monad.Freer.Extras          as Extras
import Data.Default                        (Default (..))
import Data.Functor                        (void)
import qualified Data.Map                  as Map
import Prelude                             (IO, Show(..))

import PlutusTx.Prelude                    
import Plutus.Trace                        
import Wallet.Emulator.Wallet              (knownWallet, mockWalletPaymentPubKeyHash, Wallet(..))
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Ledger.Ada                as Ada
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Ledger.TimeSlot           as TimeSlot

import qualified EvenOdd.OffChain          as OffChain
import EvenOdd.OnChain                     (GameChoice(..))

-- ----------------------------------------------------------------------
-- Main test
-- ----------------------------------------------------------------------

test :: IO ()
test = do
    --test' Zero Zero
    --test' Zero One
    --test' One Zero
    test' One One

-- ----------------------------------------------------------------------
-- Helper functions
-- ----------------------------------------------------------------------

gameTokenCurrency :: LedgerApiV2.CurrencySymbol
gameTokenCurrency = "b89bbdd6b7e801b90fbd3a249f462fb049d43dd7a87d74a71cc97369"

gameTokenName :: LedgerApiV2.TokenName
gameTokenName = "STATE TOKEN"

w1, w2 :: Wallet
w1 = knownWallet 1
w2 = knownWallet 2

test' :: GameChoice -> GameChoice -> IO ()
test' c1 c2 = runEmulatorTraceIO' def emCfg $ myTrace c1 c2
  where
    -- Wallet 1 starts with the NFT
    -- Wallet 1 and 2 start with 1000 ADA
    emCfg :: Emulator.EmulatorConfig
    emCfg = def {
      Emulator._initialChainState = Left $ Map.fromList
        [ (w1, v <> Value.assetClassValue (Value.AssetClass (gameTokenCurrency, gameTokenName)) 1)
        , (w2, v)
        ]
      }

    v :: Value.Value
    v = Ada.lovelaceValueOf 1_000_000_000

-- ----------------------------------------------------------------------
-- Emulator trace 
-- ----------------------------------------------------------------------

myTrace :: GameChoice -> GameChoice -> Emulator.EmulatorTrace ()
myTrace c1 c2 = do
  Extras.logInfo $ "first move: " ++ show c1 ++ ", second move: " ++ show c2

  h1 <- Emulator.activateContractWallet w1 OffChain.endpoints
  h2 <- Emulator.activateContractWallet w2 OffChain.endpoints

  let pkh1      = mockWalletPaymentPubKeyHash w1
      pkh2      = mockWalletPaymentPubKeyHash w2
      stake     = 100_000_000
      deadline1 = TimeSlot.slotToBeginPOSIXTime def 5
      deadline2 = TimeSlot.slotToBeginPOSIXTime def 10

      fp = OffChain.FirstParams
                { OffChain.fpSecond         = pkh2
                , OffChain.fpStake          = stake
                , OffChain.fpPlayDeadline   = deadline1
                , OffChain.fpRevealDeadline = deadline2
                , OffChain.fpNonce          = "SECRETNONCE"
                , OffChain.fpCurrency       = gameTokenCurrency
                , OffChain.fpTokenName      = gameTokenName
                , OffChain.fpChoice         = c1
                }
      sp = OffChain.SecondParams
                { OffChain.spFirst          = pkh1
                , OffChain.spStake          = stake
                , OffChain.spPlayDeadline   = deadline1
                , OffChain.spRevealDeadline = deadline2
                , OffChain.spCurrency       = gameTokenCurrency
                , OffChain.spTokenName      = gameTokenName
                , OffChain.spChoice         = c2
                }

  Emulator.callEndpoint @"first" h1 fp
  void $ Emulator.waitNSlots 3

  Emulator.callEndpoint @"second" h2 sp
  void $ Emulator.waitNSlots 10

