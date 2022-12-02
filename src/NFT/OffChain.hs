{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NumericUnderscores  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module NFT.OffChain where

import  Control.Monad                      (void, unless, when)
import qualified GHC.Generics              as GHCGenerics (Generic)
import qualified Data.Aeson                as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema       as DataOpenApiSchema (ToSchema)
import qualified Prelude                   as P
import Data.Void                           (Void)
import qualified Data.Map                  as Map
import Data.Text                           (Text, replicate)
import qualified Data.Text                 as T
import Text.Printf                         (printf)

import qualified PlutusTx                  
import PlutusTx.Prelude                    
import qualified Plutus.Contract           as PlutusContract
import qualified Ledger.Ada                as Ada
import qualified Ledger.Tx                 as LedgerTx
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Ledger                    (PaymentPubKeyHash, getCardanoTxId, unitRedeemer)
import qualified Ledger.Constraints        as Constraints
import qualified Plutus.V1.Ledger.Scripts  as ScriptsLedger
import qualified Plutus.V1.Ledger.Interval as LedgerIntervalV1
import qualified Plutus.V1.Ledger.Value    as Value

import qualified NFT.OnChain               as OnChain


-- ----------------------------------------------------------------------
-- Data types

data NFTParams = NFTParams
  { npToken   :: !LedgerApiV2.TokenName
  , npAddress :: !LedgerApiV2.Address
  } deriving (GHCGenerics.Generic,
              DataAeson.FromJSON,
              DataAeson.ToJSON,
              DataOpenApiSchema.ToSchema,
              P.Show)


-- ----------------------------------------------------------------------
-- Schema

type NFTSchema =
  PlutusContract.Endpoint "mint" NFTParams


-- ----------------------------------------------------------------------
-- Mint endpoint

mint :: NFTParams -> PlutusContract.Contract w NFTSchema Text ()
mint np = do
  utxos <- PlutusContract.utxosAt $ npAddress np

  case Map.keys utxos of
    [] -> PlutusContract.logError @P.String "no utxo found"
    oref : _ -> do
      let tn      = npToken np
          val     = Value.singleton (OnChain.curSymbol oref tn) tn 1
          lookups = Constraints.plutusV2MintingPolicy (OnChain.policy oref tn) P.<>
                    Constraints.unspentOutputs utxos
          tx      = Constraints.mustMintValue val P.<>
                    Constraints.mustSpendPubKeyOutput oref
      
      ledgerTx <- PlutusContract.submitTxConstraintsWith @Void lookups tx
      void $ PlutusContract.awaitTxConfirmed (Ledger.getCardanoTxId ledgerTx)
      PlutusContract.logInfo @P.String $ printf  "forged %s" (P.show val)


-- ----------------------------------------------------------------------
-- Endpoints

endpoints :: PlutusContract.Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = PlutusContract.awaitPromise $
              PlutusContract.endpoint @"mint" mint
