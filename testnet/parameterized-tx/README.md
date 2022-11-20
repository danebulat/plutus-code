# Parameterised Contract Plan

Creator:      <add address and key files here for reference>
Beneficiary:  <add address and key files here for reference>



Creator payment pub key hash:      <add creator pkh>
Beneficiary payment pub key hash:  <add beneficiary pkh>
Deadine:                           <add deadline POSIX timestamp>


## Generate Script Address 

Deprecated method:

```
cardano-cli address build-script \
  --script-fiile param.plutus \
  --testnet-magic 1 \
  --out-file param.addr
```

New method:

```
cardano-cli address built \
  --payment-script-file param.plutus \
  --testnet-magic 1 \
  --out-file param.addr
```

addr_test1wzdkhxwtpdfrkwqlud4vdaqmgdptesdg7fzf3mxm82cu5ssgf35pn

## Producing Tx

```
cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 2 \
  --change-address $(cat payment.addr) \
  --tx-in <TxHash>#<TxIx> \
  --tx-out $(cat param.addr)+40000000 \
  --tx-out-datum-hash-file paramDatum.json \
  --out-file tx.body
  
cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file payment.skey \
  --testnet-magic 2 \
  --out-file tx.signed

cardano-cli transaction submit \
  --tx-file tx.signed \
  --testnet-magic 2
```

- View UTXOs of script address to confirm that the transaction was 
  successful.

## Consuming Tx

```
cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 2 \
  --change-address $(cat 01.addr) \
  --tx-in <TxHash>#<TxIx> \             # script address 
  --tx-in-script-file param.plutus \    # the actual script 
  --tx-in-datum-file paramDatum.json \
  --tx-in-redeemer-file paramRedeemer.json \
  --tx-in-collateral <TxHash>#<TxIx> \
  --tx-out <TxHash>#<TxIx> \            # 10% given to creator
  --invalid-before <slot> \
  --required-signer-hash <key-hash> \   # explicitly specify required signature (beneficiary)
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file p1.skey \
  --testnet-magic 2 \
  --out-file tx.signed
  
cardano-cli transaction submit \
  --tx-file tx.signed \
  --testnet-magic 2
```

- View UTXOs of script, beneficiary and creator to confirm that the 
  transaction was successful.
