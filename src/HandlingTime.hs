module HandlingTime where 

import Plutus.V1.Ledger.Interval
import qualified Plutus.V2.Ledger.Api as V2LedgerAPI

import Wallet.Emulator
import Wallet.Emulator.Wallet (mockWalletAddress)
-- Wallet type 


main :: IO ()
main = do
  -- Plutus V1 Ledger API
  print $ interval (10 :: Integer) 20

  print $ member 9   $ interval (10 :: Integer) 20
  print $ member 10  $ interval (10 :: Integer) 20
  print $ member 21  $ from (30 :: Integer)
  print $ member 30  $ from (30 :: Integer)
  print $ member 100 $ from (30 :: Integer)
  print $ member 22  $ to   (30 :: Integer)

  print $ intersection (interval (10 :: Integer) 20) $ interval 18 30
  print $ contains (to (100 :: Integer)) $ interval 30 80
  print $ contains (to (101 :: Integer)) $ interval 30 80
  print $ overlaps (to (100 :: Integer)) (interval 30 101)

  -- Plutus V2 Ledger API
  print (V2LedgerAPI.always :: Interval Integer)
  print $ V2LedgerAPI.to (10 :: Integer)
  print $ V2LedgerAPI.from (10 :: Integer)
  print $ V2LedgerAPI.lowerBound (10 :: Integer)
  print $ V2LedgerAPI.strictLowerBound (10 :: Integer)

  -- wallets
  print $ knownWallet 2
  print $ mockWalletPaymentPubKeyHash $ knownWallet 1
  print $ mockWalletPaymentPubKey $ knownWallet 1
  print $ mockWalletAddress $ knownWallet 1
