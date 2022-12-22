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

module MintBurn.OffChain where 

import  Control.Monad                      (void, unless, when)
import qualified GHC.Generics              as GHCGenerics (Generic)
import qualified Data.Aeson                as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema       as DataOpenApiSchema (ToSchema)
import qualified Prelude                   as P
--import qualified Data.Maybe                as DM (isJust, fromJust)
import Data.Void                           (Void)
import qualified Data.Map                  as Map
import Data.Text                           (Text, replicate)
import qualified Data.Text                 as T
import Text.Printf                         (printf)

import qualified PlutusTx                  
import PlutusTx.Prelude                    
import qualified Plutus.Contract           as PC 
import qualified Ledger.Ada                as Ada
import qualified Ledger.Tx                 as LedgerTx
import qualified Plutus.V2.Ledger.Api      as LV2  
import qualified Ledger                    as L
import qualified Ledger.Constraints        as Constraints
import qualified Plutus.V1.Ledger.Scripts  as ScriptsLedger
import qualified Plutus.V1.Ledger.Interval as LedgerIntervalV1
import qualified Plutus.V1.Ledger.Value    as Value

import qualified MintBurn.OnChain          as OnChain

-- --------------------------------------------------------------------- 
-- Mint Params
-- --------------------------------------------------------------------- 

data MintParams = MintParams 
  { mpAddress   :: LV2.Address
  , mpTokenName :: LV2.TokenName 
  , mpQuantity  :: Integer
  , mpAction    :: Bool     -- True = mint, False = burn
  , mpUtxo      :: Maybe LV2.TxOutRef
  } deriving (P.Show, 
              GHCGenerics.Generic, 
              DataAeson.FromJSON, 
              DataAeson.ToJSON, 
              DataOpenApiSchema.ToSchema)

-- ---------------------------------------------------------------------- 
-- Schema Type
-- ---------------------------------------------------------------------- 

type MintSchema = PC.Endpoint "mint" MintParams 

-- ---------------------------------------------------------------------- 
-- Contracts
-- ---------------------------------------------------------------------- 

mint :: MintParams -> PC.Contract [LV2.TxOutRef] MintSchema Text () 
mint mp = do 
  utxos <- PC.utxosAt (mpAddress mp)
  pkh   <- PC.ownFirstPaymentPubKeyHash

  case Map.keys utxos of 
    [] -> PC.logInfo @P.String $ printf "No Utxos found"
    (oref : _) -> do 
      let 
        oref'   = if isJust $ mpUtxo mp then let (Just o) = (mpUtxo mp) in o else oref
        param   = OnChain.TokenParam { 
                    OnChain.utxo   = oref',
                    OnChain.name   = mpTokenName mp,
                    OnChain.minter = pkh
                  }
        val     = LV2.singleton (OnChain.curSymbol param) (mpTokenName mp) (mpQuantity mp)
        action  = if mpAction mp then OnChain.Mint else OnChain.Burn

        lookups = Constraints.plutusV2MintingPolicy (OnChain.policy param) P.<>
                  Constraints.unspentOutputs utxos
        tx      = Constraints.mustMintValueWithRedeemer 
                  (L.Redeemer $ PlutusTx.toBuiltinData action) val P.<> 
                  Constraints.mustBeSignedBy pkh
      _ <- PC.submitTxConstraintsWith @Void lookups tx 
      PC.logInfo @P.String $ printf "forged %s" (P.show val)
      PC.tell [oref]

-- ---------------------------------------------------------------------- 
-- Endpoints  
-- ---------------------------------------------------------------------- 

endpoints :: PC.Contract [LV2.TxOutRef] MintSchema Text () 
endpoints = mint' >> endpoints 
  where 
    mint' = PC.awaitPromise $ PC.endpoint @"mint" mint
