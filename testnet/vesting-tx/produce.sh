#!/bin/bash

cardano-cli transaction build \
  --babbage-era \
  --change-address <addr> \
  --tx-in <TxHash>#<TxIx> \
  --tx-out <script-addr>+10000000 \
  --tx-out-datum-hash-file vestingDatum.json \
  --out-file tx.body \
  --testnet-magic 1

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file payment.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --tx-file tx.signed \
  --testnet-magic 1
