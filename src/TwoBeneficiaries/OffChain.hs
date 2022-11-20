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
{-# LANGUAGE RecordWildCards     #-} -- To allow notation like GrabParams {..}
{-# LANGUAGE NumericUnderscores  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module TwoBeneficiaries.OffChain where


import qualified Control.Monad             as Monad (void, unless)
import qualified GHC.Generics              as GHCGenerics (Generic)
import qualified Data.Aeson                as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema       as DataOpenApiSchema (ToSchema)
import qualified Prelude                   as P
import qualified Data.Void                 as Void (Void)
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

import qualified TwoBeneficiaries.OnChain  as OnChain


-- ----------------------------------------------------------------------
-- Data types

data GiveParams = GiveParams
  { gpBeneficiary :: !Ledger.PaymentPubKeyHash
  , gpDeadline    :: !LedgerApiV2.POSIXTime
  , gpAmount      :: !Integer
  } deriving (GHCGenerics.Generic,
              DataAeson.ToJSON,
              DataAeson.FromJSON,
              DataOpenApiSchema.ToSchema)

data DeadlineFlag =
  AfterDeadline | BeforeDeadline
  deriving (P.Eq)


-- ----------------------------------------------------------------------
-- Give endpoint

give :: forall w s e. PlutusContract.AsContractError e
     => GiveParams
     -> PlutusContract.Contract w s e ()
give gp = do
    pkh <- PlutusContract.ownFirstPaymentPubKeyHash

    let dat     = OnChain.Dat {
                    OnChain.beneficiary1 = pkh,
                    OnChain.beneficiary2 = gpBeneficiary gp,
                    OnChain.deadline     = gpDeadline gp
                  }
        
        lookups = Constraints.plutusV2OtherScript OnChain.validator
        tx      = Constraints.mustPayToOtherScript
                    (OnChain.validatorHash)
                    (LedgerApiV2.Datum   $ PlutusTx.toBuiltinData dat)
                    (Ada.lovelaceValueOf $ gpAmount gp)
    
    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.VTypes lookups tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx


-- ----------------------------------------------------------------------
-- Grab endpoint

grab :: forall w s e. PlutusContract.AsContractError e
     => PlutusContract.Contract w s e ()
grab = do
    PlutusContract.logInfo @P.String $ printf ">>> Starting grab..."
    now   <- PlutusContract.currentTime
    pkh   <- PlutusContract.ownFirstPaymentPubKeyHash
    utxos <- PlutusContract.utxosAt OnChain.address

    PlutusContract.logInfo @P.String $ printf ">>> All UTXO length: %d" (P.length utxos)
    PlutusContract.logInfo @P.String $ printf ">>> Now: %d" $ LedgerApiV2.getPOSIXTime now

    let utxos1 = Map.filter (isSuitable $ \dat ->
                   OnChain.beneficiary1 dat == pkh && now <= OnChain.deadline dat) utxos
        utxos2 = Map.filter (isSuitable $ \dat ->
                   OnChain.beneficiary2 dat == pkh && now > OnChain.deadline dat) utxos

    PlutusContract.logInfo @P.String $ printf ">>> UTXO length 1: %d" (P.length utxos1)
    PlutusContract.logInfo @P.String $ printf ">>> UTXO length 2: %d" (P.length utxos2)

    Monad.unless (Map.null utxos1) $ spendUtxos utxos1 now BeforeDeadline
    Monad.unless (Map.null utxos2) $ spendUtxos utxos2 now AfterDeadline
  where
    isSuitable :: (OnChain.Dat -> Bool) -> LedgerTx.ChainIndexTxOut -> Bool
    isSuitable p o = let (_, mDat) = LedgerTx._ciTxOutScriptDatum o in
      case mDat of
        Nothing  -> False
        Just dat -> let maybeDat = PlutusTx.fromBuiltinData (LedgerApiV2.getDatum dat)
                    in maybe False p maybeDat
                        


-- ----------------------------------------------------------------------
-- Spend the suitable UTXOs

spendUtxos :: forall w s e. PlutusContract.AsContractError e
           => Map.Map LedgerTx.TxOutRef LedgerTx.ChainIndexTxOut
           -> LedgerApiV2.POSIXTime
           -> DeadlineFlag
           -> PlutusContract.Contract w s e ()
spendUtxos utxos now f = do
  PlutusContract.logInfo @P.String $ printf ">>> Starting spend..."
  let orefs     = fst <$> Map.toList utxos
      interval' = --LedgerIntervalV1.interval now (now + 10_000) -- 10 second tx interval
        if f P.== BeforeDeadline
                    then LedgerIntervalV1.to now
                    else LedgerIntervalV1.from now
      lookups   = Constraints.unspentOutputs utxos P.<>
                  Constraints.plutusV2OtherScript OnChain.validator

      tx :: Constraints.TxConstraints () OnChain.Dat
      tx = mconcat [Constraints.mustSpendScriptOutput oref Ledger.unitRedeemer
                   | oref <- orefs] P.<>
                   Constraints.mustValidateIn interval'
  Monad.void $ PlutusContract.submitTxConstraintsWith @OnChain.VTypes lookups tx


-- ----------------------------------------------------------------------
-- Endpoints 

type TwoBeneficiariesSchema =
            PlutusContract.Endpoint "give" GiveParams
            PlutusContract..\/
            PlutusContract.Endpoint "grab" ()


endpoints :: PlutusContract.Contract () TwoBeneficiariesSchema Text ()
endpoints = do
    PlutusContract.awaitPromise (give' `PlutusContract.select` grab')
    endpoints
  where
    give' = PlutusContract.endpoint @"give" give
    grab' = PlutusContract.endpoint @"grab" $ const grab

