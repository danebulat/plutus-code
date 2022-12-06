-- Extensions
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

-- Required to use custom data types
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Week07.StateMachine.OnChain where

import qualified GHC.Generics                                    as GHCGenerics (Generic)
import qualified Data.Aeson                                      as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                             as DataOpenApiSchema (ToSchema)

import PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Address                                  as LAddressV1
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.Script.Utils.V2.Scripts                  as UtilsScriptsV2
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as UtilsTypeScriptsV2
import qualified Plutus.Script.Utils.V2.Typed.Scripts            as Scripts
import qualified Prelude                                         as P
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1
import qualified Plutus.V1.Ledger.Value                          as Value
import qualified Ledger.Ada                                      as Ada
import qualified Ledger
import Ledger.Value (Value(..))
import qualified Ledger.Interval as Interval

import qualified Plutus.Contract              as PlutusContract
import qualified Plutus.Contract.StateMachine as StateMachine
import qualified Ledger.Constraints           as Constraints
import Data.Void (Void)

-- ----------------------------------------------------------------------
-- Game

data Game = Game
  { gFirst          :: Ledger.PaymentPubKeyHash
  , gSecond         :: Ledger.PaymentPubKeyHash
  , gStake          :: Integer
  , gPlayDeadline   :: Ledger.POSIXTime
  , gRevealDeadline :: Ledger.POSIXTime
  , gToken          :: StateMachine.ThreadToken
  } deriving (P.Show, P.Eq,
              GHCGenerics.Generic,
              DataAeson.ToJSON,
              DataAeson.FromJSON)

PlutusTx.makeLift ''Game

-- ----------------------------------------------------------------------
-- GameChoice

data GameChoice = Zero | One
  deriving (P.Show,
            GHCGenerics.Generic,
            DataAeson.ToJSON,
            DataAeson.FromJSON)

instance Eq GameChoice where
  {-# INLINEABLE (==) #-}
  Zero == Zero = True
  One  == One  = True
  _    == _    = False
  
PlutusTx.unstableMakeIsData ''GameChoice

-- ----------------------------------------------------------------------
-- GameDatum

data GameDatum
    = GameDatum BuiltinByteString (Maybe GameChoice)
    | Finished
    deriving (P.Show)

instance Eq GameDatum where
  {-# INLINEABLE (==) #-}
  GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
  Finished        == Finished          = True
  _               == _                 = False

PlutusTx.unstableMakeIsData ''GameDatum

-- ----------------------------------------------------------------------
-- GameRedeemer

data GameRedeemer
  = Play GameChoice
  | Reveal BuiltinByteString
  | ClaimFirst
  | ClaimSecond
  deriving (P.Show)

PlutusTx.unstableMakeIsData ''GameRedeemer

-- ----------------------------------------------------------------------
-- Helper functions

{-# INLINEABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINEABLE gameDatum #-}
gameDatum :: Maybe LedgerApiV2.Datum -> Maybe GameDatum
gameDatum md = do
  LedgerApiV2.Datum d <- md
  PlutusTx.fromBuiltinData d

-- ----------------------------------------------------------------------
-- StateMachine instance

{-# INLINEABLE transition #-}
transition
    :: Game
    -> StateMachine.State GameDatum
    -> GameRedeemer
    -> Maybe (StateMachine.TxConstraints Void Void, StateMachine.State GameDatum)
transition game s r =
  case (StateMachine.stateValue s, StateMachine.stateData s, r) of

    -- Play (2nd player)
    (v, GameDatum bs Nothing, Play c) | lovelaces v == gStake game ->
       Just ( Constraints.mustBeSignedBy (gSecond game) P.<>
              Constraints.mustValidateIn (Interval.to $ gPlayDeadline game)
            , StateMachine.State (GameDatum bs $ Just c) (Ada.lovelaceValueOf $ 2 * gStake game))

    -- Reveal
    (v, GameDatum _ (Just _), Reveal _) | lovelaces v == (2 * gStake game) ->
      Just ( Constraints.mustBeSignedBy (gFirst game) P.<>
             Constraints.mustValidateIn (Interval.to $ gRevealDeadline game)
           , StateMachine.State Finished mempty)

    -- ClaimFirst
    (v, GameDatum _ Nothing, ClaimFirst) | lovelaces v == gStake game ->
      Just ( Constraints.mustBeSignedBy (gFirst game) P.<>
             Constraints.mustValidateIn (Interval.from $ 1 + gPlayDeadline game)
           , StateMachine.State Finished mempty)

    -- ClaimSecond
    (v, GameDatum _ (Just _), ClaimSecond) | lovelaces v == (2 * gStake game) ->
      Just ( Constraints.mustBeSignedBy (gSecond game) P.<>
             Constraints.mustValidateIn (Interval.from $ 1 + gRevealDeadline game)
           , StateMachine.State Finished mempty)
      
    -- Invalid tx
    _ -> Nothing

{-# INLINABLE final #-}
final :: GameDatum -> Bool
final Finished = True
final _        = False

{-# INLINEABLE check' #-}
check'
    :: BuiltinByteString
    -> BuiltinByteString
    -> GameDatum
    -> GameRedeemer
    -> Contexts.ScriptContext
    -> Bool
check' bsZero' bsOne' (GameDatum bs (Just c)) (Reveal nonce) _ =
   sha2_256 (nonce `appendByteString` if c == Zero then bsZero' else bsOne' ) == bs
check' _ _ _ _ _ = True


-- ----------------------------------------------------------------------
-- Return StateMachine instance 

gameStateMachine
    :: Game
    -> BuiltinByteString
    -> BuiltinByteString
    -> StateMachine.StateMachine GameDatum GameRedeemer
gameStateMachine game bsZero' bsOne' = StateMachine.StateMachine
  { StateMachine.smTransition  = transition game
  , StateMachine.smFinal       = final
  , StateMachine.smCheck       = check' bsZero' bsOne'
  , StateMachine.smThreadToken = Just $ gToken game
  }


-- ----------------------------------------------------------------------
-- Boilerplate

mkGameValidator
    :: Game
    -> BuiltinByteString
    -> BuiltinByteString
    -> GameDatum
    -> GameRedeemer
    -> Contexts.ScriptContext
    -> Bool 
mkGameValidator game bsZero' bsOne' =
  StateMachine.mkValidator $ gameStateMachine game bsZero' bsOne'

type Gaming = StateMachine.StateMachine GameDatum GameRedeemer

bsZero, bsOne :: BuiltinByteString
bsZero = "0"
bsOne  = "1"

gameStateMachine' :: Game -> StateMachine.StateMachine GameDatum GameRedeemer
gameStateMachine' game = gameStateMachine game bsZero bsOne

typedGameValidator :: Game -> UtilsTypeScriptsV2.TypedValidator Gaming
typedGameValidator game = UtilsTypeScriptsV2.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = UtilsTypeScriptsV2.mkUntypedValidator @GameDatum @GameRedeemer

gameValidator :: Game -> LedgerApiV2.Validator
gameValidator = Scripts.validatorScript . typedGameValidator

-- gameAddress :: Game -> Ledger.Address
-- gameAddress = scriptAddress . gameValidator
