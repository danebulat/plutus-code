{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NumericUnderscores  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Week06.OffChain where

import Control.Monad                       (void, unless, when)
import qualified GHC.Generics              as GHCGenerics (Generic)
import qualified Data.Aeson                as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema       as DataOpenApiSchema (ToSchema)
import qualified Prelude                   as P
import Data.Void                           (Void)
import qualified Data.Map                  as Map
import Data.Maybe                          (fromJust)
import Data.Text                           (Text, replicate)
import qualified Data.Text                 as T
import Text.Printf                         (printf)
                                           
import qualified PlutusTx                  
import PlutusTx.Prelude                    
import qualified Plutus.Contract           as PlutusContract
import qualified Ledger.Ada                as Ada
import qualified Ledger.Tx                 as LedgerTx
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import qualified Ledger                    (PaymentPubKeyHash, getCardanoTxId, unitRedeemer)
import qualified Ledger.Constraints        as Constraints
import qualified Plutus.V1.Ledger.Scripts  as ScriptsLedger
import qualified Plutus.V1.Ledger.Interval as LedgerIntervalV1
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Plutus.Contract.Wallet    as ContractWallet

import qualified Week06.OnChain            as OnChain
import Week06.Utils                        (getCredentials)

-- ----------------------------------------------------------------------
-- Data types

data TokenParams = TokenParams
  { tpToken   :: !LedgerApiV2.TokenName
  , tpAmount  :: !Integer
  , tpAddress :: !LedgerApiV2.Address
  } deriving (P.Eq, P.Ord, P.Show,
              GHCGenerics.Generic,
              DataAeson.FromJSON,
              DataAeson.ToJSON,
              DataOpenApiSchema.ToSchema)


-- ----------------------------------------------------------------------
-- Helper function to handle min-ada value (balancing) and submit tx

adjustAndSubmitWith
    :: ( PlutusTx.FromData (Scripts.DatumType a)
       , PlutusTx.ToData   (Scripts.RedeemerType a)
       , PlutusTx.ToData   (Scripts.DatumType a)
       , PlutusContract.AsContractError e
       )
    => Constraints.ScriptLookups a
    -> Constraints.TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
    -> PlutusContract.Contract w s e LedgerTx.CardanoTx
adjustAndSubmitWith lookups constraints = do

  -- mkTxConstraints: Build a tx that satisfies the constraints
  -- adjustUnbalancedTx: Adjust the unbalanced tx
  
  unbalanced <- PlutusContract.mkTxConstraints lookups constraints >>=
                PlutusContract.adjustUnbalancedTx
  PlutusContract.logDebug @P.String $ printf "unbalanced: %s" $ P.show unbalanced

  -- balanceTx:
  -- Send an unbalanced tx to be balanced. Returns the balanced tx. Throws
  -- an error if balancing failed.
  
  unsigned <- PlutusContract.balanceTx unbalanced
  PlutusContract.logDebug @P.String $ printf "balanced: %s" $ P.show unsigned

  -- submitBalancedTx:
  -- Send a balanced tx to be signed. Returns the ID of the final tx when the
  -- tx was submitted. Throws an error if signing failed.

  signed <- PlutusContract.submitBalancedTx unsigned
  PlutusContract.logDebug @P.String $ printf "signed: %s" $ P.show signed
  return signed
  

-- ----------------------------------------------------------------------
-- Mint endpoint

mintToken :: TokenParams -> PlutusContract.Contract w s Text ()
mintToken tp = do
  PlutusContract.logDebug @P.String $ printf "started minting %s" $ P.show tp
  let addr = tpAddress tp
  case getCredentials addr of
    Nothing -> PlutusContract.throwError $ T.pack $
                 printf "expected pubkey address, but got %s" $ P.show addr

    Just (x, my) -> do
      oref <- ContractWallet.getUnspentOutput
      o    <- fromJust <$> PlutusContract.txOutFromRef oref
      PlutusContract.logDebug @P.String $ printf "picked UTxO at %s with value %s"
        (P.show oref) (P.show $ LedgerTx._ciTxOutValue o)

      let tn  = tpToken tp
          amt = tpAmount tp
          cs  = OnChain.curSymbol oref tn amt
          val = Value.singleton cs tn amt

          -- Constraint differs depending on if address has a staking component
          mustPayToConstraint = case my of
            Nothing -> Constraints.mustPayToPubKey x val          -- no staking component 
            Just y  -> Constraints.mustPayToPubKeyAddress x y val -- staking component 

          lookups     = Constraints.plutusV2MintingPolicy (OnChain.policy oref tn amt) P.<>
                        Constraints.unspentOutputs (Map.singleton oref o)
          
          constraints = Constraints.mustMintValue val P.<>
                        Constraints.mustSpendPubKeyOutput oref P.<>
                        mustPayToConstraint

      void $ adjustAndSubmitWith @Void lookups constraints
      PlutusContract.logInfo @P.String $ printf "minted %s" (P.show val)
