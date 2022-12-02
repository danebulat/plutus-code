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

module Week07.EvenOdd where

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


-- ----------------------------------------------------------------------
-- Validator parameter 

data Game = Game
  { gFirst          :: LAddressV1.PaymentPubKeyHash  -- first player 
  , gSecond         :: LAddressV1.PaymentPubKeyHash  -- second player
  , gStake          :: Integer                       -- lovelace to be paid by both players
  , gPlayDeadline   :: LedgerApiV2.POSIXTime         -- deadline for 2nd player to do tx
  , gRevealDeadline :: LedgerApiV2.POSIXTime         -- deadline for 1st player to do tx
  , gToken          :: Value.AssetClass              -- nft to track correct utxo
  } deriving (P.Show, P.Eq, P.Ord,
              GHCGenerics.Generic,
              DataAeson.FromJSON,
              DataAeson.ToJSON,
              DataOpenApiSchema.ToSchema)

PlutusTx.makeLift ''Game


-- ----------------------------------------------------------------------
-- Datum types

data GameChoice = Zero | One
  deriving (P.Show, P.Eq, P.Ord,
           GHCGenerics.Generic,
           DataAeson.FromJSON,
           DataAeson.ToJSON,
           DataOpenApiSchema.ToSchema)

instance Eq GameChoice where
  {-# INLINEABLE (==) #-}
  Zero == Zero = True
  One  == One  = True
  _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice


data GameDatum = GameDatum
    LedgerApiV2.BuiltinByteString  -- player 1 nonce+choice hash 
    (Maybe GameChoice)             -- player 2 choice
  deriving (P.Show)

instance Eq GameDatum where
  {-# INLINEABLE (==) #-}
  GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')

PlutusTx.unstableMakeIsData ''GameDatum


-- ----------------------------------------------------------------------
-- Redeemer type 

data GameRedeemer
  = Play GameChoice                       -- player 2 play tx
  | Reveal LedgerApiV2.BuiltinByteString  -- player 1 reveal tx
  | ClaimFirst                            -- player 2 claim tx
  | ClaimSecond                           -- player 1 claim tx
  deriving (P.Show)

PlutusTx.unstableMakeIsData ''GameRedeemer


-- ----------------------------------------------------------------------
-- Helper functions

{-# INLINEABLE lovelaces #-}
lovelaces :: Value.Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINEABLE gameDatum #-}
gameDatum :: Maybe LedgerApiV2.Datum -> Maybe GameDatum
gameDatum md = do
  LedgerApiV2.Datum d <- md
  PlutusTx.fromBuiltinData d

{-# INLINEABLE getUtxoDatum #-}
getUtxoDatum :: Contexts.TxOut -> GameDatum
getUtxoDatum out = case LedgerApiV2.txOutDatum out of
  LedgerApiV2.NoOutputDatum     -> traceError "no datum in script utxo"
  LedgerApiV2.OutputDatumHash _ -> traceError "need actual datum, not datum hash"
  LedgerApiV2.OutputDatum d     ->
    case PlutusTx.fromBuiltinData $ LedgerApiV2.getDatum d of
      Nothing -> traceError "error converting Datum to GameDatum"
      Just d' -> d'

{-# INLINEABLE mkGameValidator #-}
mkGameValidator
    :: Game                            -- game parameter
    -> LedgerApiV2.BuiltinByteString   -- zero bs 
    -> LedgerApiV2.BuiltinByteString   -- one bs
    -> GameDatum                       -- game state 
    -> GameRedeemer                    -- game action to process
    -> Contexts.ScriptContext          -- tx info 
    -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =

  -- Check spending utxo has NFT
    traceIfFalse "token missing from input"
      (Value.assetClassValueOf (LedgerApiV2.txOutValue ownInput) (gToken game) == 1) &&

  case (dat, red) of

    -- Player 2 plays
    (GameDatum bs Nothing, Play c) ->
      traceIfFalse "not signed by second player"
        (Contexts.txSignedBy info (LAddressV1.unPaymentPubKeyHash $ gSecond game)) &&
      traceIfFalse "first player's stake missing"
        (lovelaces (LedgerApiV2.txOutValue ownInput) == gStake game) &&
      traceIfFalse "second player's stake missing"
        (lovelaces (LedgerApiV2.txOutValue ownOutput) == (2 * gStake game)) &&
      traceIfFalse "wrong output datum"
        (outputDatum == GameDatum bs (Just c)) &&
      traceIfFalse "missed deadline"
        (LedgerIntervalV1.to (gPlayDeadline game) `LedgerIntervalV1.contains`
         Contexts.txInfoValidRange info) &&
      traceIfFalse "token missing from output"
        (Value.assetClassValueOf (Contexts.txOutValue ownOutput) (gToken game) == 1)

    -- Player 1 reveals their choice
    -- TODO: Don't assume 'c' is same as player 1 choice.
    --       Have player 1 re-send their choice and use that in hash.
    (GameDatum bs (Just c), Reveal nonce) ->
      traceIfFalse "not signed by first player"
        (Contexts.txSignedBy info (LAddressV1.unPaymentPubKeyHash $ gFirst game)) &&
      traceIfFalse "commit mismatch"
        (checkNonce bs nonce c) &&
      traceIfFalse "missed deadline"
        (LedgerIntervalV1.to (gRevealDeadline game) `LedgerIntervalV1.contains`
         Contexts.txInfoValidRange info) &&
      traceIfFalse "wrong stake"
        (lovelaces (Contexts.txOutValue ownInput) == (2 * gStake game)) &&
      traceIfFalse "NFT must go to first player" nftToFirst

    -- Player 1 claims (player 2 didn't play)
    (GameDatum _ Nothing, ClaimFirst) ->
      traceIfFalse "not signed by first player"
        (Contexts.txSignedBy info (LAddressV1.unPaymentPubKeyHash $ gFirst game)) &&
      traceIfFalse "too early"
        (LedgerIntervalV1.from (1 + gPlayDeadline game) `LedgerIntervalV1.contains`
         Contexts.txInfoValidRange info) &&
      traceIfFalse "first player's stake missing"
        (lovelaces (Contexts.txOutValue ownInput) == gStake game) &&
      traceIfFalse "NFT must go to first player" nftToFirst
        
    -- Player 2 claims (player 1 didn't reveal)
    (GameDatum _ (Just _), ClaimSecond) ->
      traceIfFalse "not signed by second player"
        (Contexts.txSignedBy info (LAddressV1.unPaymentPubKeyHash $ gSecond game)) &&
      traceIfFalse "too early"
        (LedgerIntervalV1.from (1 + gRevealDeadline game) `LedgerIntervalV1.contains`
         Contexts.txInfoValidRange info) &&
      traceIfFalse "wrong stake"
        (lovelaces (Contexts.txOutValue ownInput) == (2 * gStake game)) &&
      traceIfFalse "NFT must go to first player" nftToFirst

    -- Otherwise invalid transaction
    _ -> False
  where 
    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    -- Extract TxOut from TxInInfo (the script utxo being spent)
    ownInput :: LedgerApiV2.TxOut
    ownInput = case Contexts.findOwnInput ctx of
      Nothing -> traceError "game input missing"
      Just i  -> Contexts.txInInfoResolved i

    -- Get all the outputs that pay to the same script address we are currently
    -- spending from, if any.
    ownOutput :: LedgerApiV2.TxOut
    ownOutput = case Contexts.getContinuingOutputs ctx of
      [o] -> o
      _   -> traceError "expected exactly one game output"
    
    -- Extract datum from the script's latest unspent utxo
    outputDatum :: GameDatum
    outputDatum = getUtxoDatum ownOutput

    -- Check if hash(nonce||choice) matches first hash
    checkNonce
        :: LedgerApiV2.BuiltinByteString   -- hash stored in datum 
        -> LedgerApiV2.BuiltinByteString   -- nonce submitted by player 1 for reveal
        -> GameChoice                      -- choice submitted by player 1 for reveal
        -> Bool                            -- True if hash(nonce||choice) == bs
    checkNonce bs nonce cSecond =
        sha2_256 (nonce `appendByteString` cFirst) == bs
      where
        -- Convert GameChoice to BuiltinByteString
        cFirst :: LedgerApiV2.BuiltinByteString
        cFirst = case cSecond of
          Zero -> bsZero'
          One  -> bsOne'

    -- Check the NFT is sent to player 1 (after game)
    nftToFirst :: Bool
    nftToFirst = Value.assetClassValueOf (Contexts.valuePaidTo info ppkh)
                   (gToken game) == 1
      where ppkh = LAddressV1.unPaymentPubKeyHash $ gFirst game


-- ----------------------------------------------------------------------
-- Boilerplate

data Gaming
instance UtilsTypeScriptsV2.ValidatorTypes Gaming where
  type instance DatumType    Gaming = GameDatum
  type instance RedeemerType Gaming = GameRedeemer

bsZero, bsOne :: LedgerApiV2.BuiltinByteString
bsZero = "0"
bsOne  = "1"

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
gameValidator = UtilsTypeScriptsV2.validatorScript . typedGameValidator

validatorHash :: Game -> LedgerApiV2.ValidatorHash
validatorHash = UtilsTypeScriptsV2.validatorHash . typedGameValidator

gameAddress :: Game -> LAddressV1.Address
gameAddress = LAddressV1.scriptHashAddress . validatorHash

