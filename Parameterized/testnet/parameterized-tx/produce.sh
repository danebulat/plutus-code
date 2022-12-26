#!/bin/bash

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 1 \
  --change-address <creator-addr> \
  --tx-in <TxHash>#<TxIx> \
  --tx-out addr_test1wzdkhxwtpdfrkwqlud4vdaqmgdptesdg7fzf3mxm82cu5ssgf35pn+10000000 \
  --tx-out-datum-hash-file paramDatum.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file <creator-skey-file> \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --tx-file tx.signed \
  --testnet-magic 1
