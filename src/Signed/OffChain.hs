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

module Signed.OffChain where

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

import qualified Signed.OnChain            as OnChain


-- ----------------------------------------------------------------------
-- Data types 

data MintParams = MintParams
  { mpTokenName :: !LedgerApiV2.TokenName
  , mpAmount    :: !Integer
  } deriving (GHCGenerics.Generic,
              DataAeson.ToJSON,
              DataAeson.FromJSON,
              DataOpenApiSchema.ToSchema)


-- ----------------------------------------------------------------------
-- Schema

type FreeSchema =
  PlutusContract.Endpoint "mint" MintParams


-- ----------------------------------------------------------------------
-- Mint endpoint

mint :: MintParams -> PlutusContract.Contract w FreeSchema T.Text ()
mint mp = do
  pkh <- PlutusContract.ownFirstPaymentPubKeyHash
  PlutusContract.logInfo @P.String $ printf "Using pkh: %s" (P.show pkh)

  let val     = Value.singleton (OnChain.curSymbol pkh) (mpTokenName mp) (mpAmount mp)
      lookups = Constraints.plutusV2MintingPolicy (OnChain.policy pkh)
      tx      = Constraints.mustMintValueWithRedeemer Ledger.unitRedeemer val P.<>
                Constraints.mustBeSignedBy pkh

  ledgerTx <- PlutusContract.submitTxConstraintsWith @Void lookups tx
  void $ PlutusContract.awaitTxConfirmed (Ledger.getCardanoTxId ledgerTx)
  PlutusContract.logInfo @P.String $ printf "forged %s" (P.show val)


-- ----------------------------------------------------------------------
-- Endpoints 

endpoints :: PlutusContract.Contract () FreeSchema T.Text ()
endpoints = mint' >> endpoints
  where
    mint' = PlutusContract.awaitPromise $
              PlutusContract.endpoint @"mint" mint

