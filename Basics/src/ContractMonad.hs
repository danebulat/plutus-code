{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ContractMonad where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-- Contract w s e a
-- EmulatorTrace a

-- -------------------------------------------------------------------
-- contract1 - Throwing an exception
-- -------------------------------------------------------------------

-- Empty = no endpoints
-- Contract.logInfo can take anything that can be serialized as JSON

contract1 :: Contract () Empty Text ()
contract1 = do
  void $ Contract.throwError "BOOM!"
  Contract.logInfo @String "hello from the contract"

trace1 :: EmulatorTrace ()
trace1 = void $ activateContractWallet (knownWallet 1) contract1

test1 :: IO ()
test1 = runEmulatorTraceIO trace1

-- -------------------------------------------------------------------
-- contract2 - Handling exceptions
-- -------------------------------------------------------------------

-- Void as error type has no values (constructors)
-- means this contract can't throw an exception (no value of type Void)

contract2 :: Contract () Empty Void ()
contract2 = do
  -- run a computation and handle an exception if one is thrown
  -- handleError can also be handled with Maybe and Either monads
  Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err)  -- exception handler function
    contract1                                               -- contract we want to run

trace2 :: EmulatorTrace ()
trace2 = void $ activateContractWallet (knownWallet 1) contract1

test2 :: IO ()
test2 = runEmulatorTraceIO trace2

-- -------------------------------------------------------------------
-- MyContract3 - schema / endpoints
-- -------------------------------------------------------------------

-- Invoke endpoints from outside contracts and provide data to the
-- contracts.

type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String

contract3 :: Contract () MySchema Text ()
contract3 = do
  awaitPromise $ endpoint @"foo" Contract.logInfo -- apply to Int
  awaitPromise $ endpoint @"bar" Contract.logInfo -- apply to String

trace3 :: EmulatorTrace ()
trace3 = do
  -- now we need the handle to the contract
  h <- activateContractWallet (knownWallet 1) contract3
  -- invoke an endpoint
  Emulator.callEndpoint @"foo" h 42
  Emulator.callEndpoint @"bar" h "Haskell"

test3 :: IO ()
test3 = runEmulatorTraceIO trace3

-- -------------------------------------------------------------------
-- MyContract4 - using `w` parameter
-- -------------------------------------------------------------------

myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
  void $ Contract.waitNSlots 10
  tell [1]
  void $ Contract.waitNSlots 10
  tell [2]
  void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
  h <- activateContractWallet (knownWallet 1) myContract4
  void $ Emulator.waitNSlots 5

  -- pass handle of contract to the one we want to observe
  xs <- observableState h

  -- output log info from contract4
  Extras.logInfo $ show xs

  void $ Emulator.waitNSlots 10
  ys <- observableState h

  Extras.logInfo $ show ys

  void $ Emulator.waitNSlots 10
  zs <- observableState h

  Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
