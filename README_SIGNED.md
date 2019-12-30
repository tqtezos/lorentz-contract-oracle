
See [`README.md`](README.md) for more details, including how to originate the oracle contract.

This tutorial explains how to originate and use the signed admin contract
to administer the oracle.

Instead of the admin having the only address that can make changes, any address
can send a transaction with an update signed by an admin _key_.

## Originating the admin contract

```bash
❯❯❯ alpha-client --wait none originate contract NatOracleSignedAdmin \
  transferring 0 from $ALICE_ADDRESS running \
  "$(./stack exec -- lorentz-contract-oracle SignedAdmin print --valueType "nat" --oneline)" \
  --init "$(./stack exec -- lorentz-contract-oracle SignedAdmin init \
  --oracleContract $ORACLE_ADDRESS \
  --admin "$(get_public_key alice)")" --burn-cap 0.938

Waiting for the node to be bootstrapped before injection...
Current head: BM3aWMUKwbfn (timestamp: 2019-12-30T19:24:52-00:00, validation: 2019-12-30T19:25:06-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 27616 units (will add 100 for safety)
Estimated storage: 938 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooJW7WXxCVJ4TLspWKZmwHmeeDGtEjnVxVCKtBC23a8Amuu6Xxc'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooJW7WXxCVJ4TLspWKZmwHmeeDGtEjnVxVCKtBC23a8Amuu6Xxc to be included --confirmations 30 --branch BM3aWMUKwbfnYrQ3KVCx43pd8WMAaKuHzoMRNmyWBWuyfo5x8jJ
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0.003718
    Expected counter: 54708
    Gas limit: 27716
    Storage limit: 958 bytes
    Balance updates:
      tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ............ -ꜩ0.003718
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,83) ... +ꜩ0.003718
    Origination:
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      Credit: ꜩ0
      Script:
        { parameter (pair signature (pair nat (or (or nat address) (or address key)))) ;
          storage (pair nat (pair key address)) ;
          code { DUP ;
                 CAR ;
                 DIP { CDR } ;
                 DIP { DUP ; DUP ; CAR ; DIP { CDR } ; SWAP ; CAR } ;
                 DUP ;
                 DIP { SWAP ;
                       DIP { DUP ; CAR ; DIP { CDR } ; DIP { PACK } } ;
                       CHECK_SIGNATURE ;
                       IF {} { PUSH string "invalid signature" ; FAILWITH } } ;
                 CDR ;
                 DUP ;
                 CAR ;
                 DIP { CDR } ;
                 SWAP ;
                 DIP { COMPARE ; EQ ; IF {} { PUSH string "unequal counters" ; FAILWITH } } ;
                 DIP { DUP ; CAR ; DIP { CDR } ; PUSH nat 1 ; ADD ; PAIR } ;
                 IF_LEFT
                   { IF_LEFT
                       { DIP { DUP ; CDR ; CDR } ;
                         LEFT address ;
                         RIGHT (pair unit (contract nat)) ;
                         DIP { CONTRACT (or (pair unit (contract nat)) (or nat address)) ;
                               IF_NONE { PUSH string "not Oracle" ; FAILWITH } {} ;
                               PUSH mutez 0 } ;
                         TRANSFER_TOKENS ;
                         DIP { NIL operation } ;
                         CONS ;
                         PAIR }
                       { DIP { DUP ; CDR ; CDR } ;
                         RIGHT nat ;
                         RIGHT (pair unit (contract nat)) ;
                         DIP { CONTRACT (or (pair unit (contract nat)) (or nat address)) ;
                               IF_NONE { PUSH string "not Oracle" ; FAILWITH } {} ;
                               PUSH mutez 0 } ;
                         TRANSFER_TOKENS ;
                         DIP { NIL operation } ;
                         CONS ;
                         PAIR } }
                   { IF_LEFT
                       { SWAP ;
                         DUP ;
                         CAR ;
                         DIP { CDR } ;
                         DIP { CAR } ;
                         DIP { PAIR } ;
                         PAIR ;
                         NIL operation ;
                         PAIR }
                       { SWAP ;
                         DUP ;
                         CAR ;
                         DIP { CDR } ;
                         DIP { CDR ; SWAP } ;
                         DIP { PAIR } ;
                         PAIR ;
                         NIL operation ;
                         PAIR } } } }
        Initial storage:
          (Pair 0
                (Pair "edpkvCHgVArnZo9RTP4P6euLTyhE89u73CYjBgsP4wEJbj4quao9oR"
                      "KT1VTqmma3vCH9nkLL1Jakd6MiUwxwqieXDE"))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1UvdRG22BE96VJMfECHn5uEtjGFWsKV1BU
        Storage size: 681 bytes
        Paid storage size diff: 681 bytes
        Consumed gas: 27616
        Balance updates:
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.681
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.257

New contract KT1UvdRG22BE96VJMfECHn5uEtjGFWsKV1BU originated.
Contract memorized as NatOracleSignedAdmin.
```

Then make a `bash` alias for the contract:

```bash
SIGNED_ADMIN_ADDRESS="KT1UvdRG22BE96VJMfECHn5uEtjGFWsKV1BU"
```

## Set the oracle's admin to the new contract

You'll need an oracle contract that stores `nat` values:
see [`README.md`](README.md) for more details.

Once you have the oracle contract's address set to `ORACLE_ADDRESS`,
update the oracle to be administered by the new admin contract:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $ALICE_ADDRESS to $ORACLE_ADDRESS \
  --arg "$(./stack exec -- lorentz-contract-oracle Oracle update-admin \
  --admin $SIGNED_ADMIN_ADDRESS)" --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BKvfzoBrgTCX (timestamp: 2019-12-30T19:35:52-00:00, validation: 2019-12-30T19:35:59-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 18330 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'onzC3utbHoCpGjQpsK5HDaH8BCoxxUfv56ArQ5Tqe4rDqs9DKHy'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for onzC3utbHoCpGjQpsK5HDaH8BCoxxUfv56ArQ5Tqe4rDqs9DKHy to be included --confirmations 30 --branch BKvfzoBrgTCXpkvkcHP5vpFuMEwaMdBZ4yQpmHMiZA32t8FvJzx
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0.002144
    Expected counter: 54727
    Gas limit: 18430
    Storage limit: 0 bytes
    Balance updates:
      tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ............ -ꜩ0.002144
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,83) ... +ꜩ0.002144
    Transaction:
      Amount: ꜩ0
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      To: KT1VTqmma3vCH9nkLL1Jakd6MiUwxwqieXDE
      Parameter: (Right (Right "KT1UvdRG22BE96VJMfECHn5uEtjGFWsKV1BU"))
      This transaction was successfully applied
      Updated storage:
        (Pair 4 0x01df2408d0767b08295b38e2fc0d49bee0cf7652ee00)
      Storage size: 355 bytes
      Consumed gas: 18330
```


## Updating the oracle value

If you want to sign the Michelson value without the CLI tool, elide a secret key:

```bash
❯❯❯ ./stack exec -- lorentz-contract-oracle SignedAdmin update-value --newValueType "nat" --newValue 7 
Left (Left 7)
```

Otherwise, you'll need to provide the current counter and secret key:

```bash
❯❯❯ ./stack exec -- lorentz-contract-oracle SignedAdmin update-value --newValueType "nat" --newValue 7 --counter 0 --secretKey "$(get_secret_key alice)"
Pair "edsigtm7116sRJH8S3PW8N2TxJinC9cP3LmEKn7DZ1wJzQxaqHo6gZq9cCvRgTJzHKA2XxoqnkJjZMjkefPMk523Hn5dhhLUZJQ" (Pair 0 (Left (Left 7)))
```

To update the value (assuming you provide the secret key directly):

```bash
❯❯❯ alpha-client --wait none transfer 0 from $ALICE_ADDRESS to $SIGNED_ADMIN_ADDRESS \
  --arg "$(./stack exec -- lorentz-contract-oracle SignedAdmin update-value \
  --newValueType "nat" \
  --newValue 7 \
  --counter 0 \
  --secretKey "$(get_secret_key alice)")" --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BMYxeM21yc1s (timestamp: 2019-12-30T19:39:22-00:00, validation: 2019-12-30T19:39:36-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 60138 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'opHZA29PYcDHwshSQrtpnY9pcGKSxU7mYK7PpBKEbQw7wkBP1vU'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opHZA29PYcDHwshSQrtpnY9pcGKSxU7mYK7PpBKEbQw7wkBP1vU to be included --confirmations 30 --branch BMYxeM21yc1sFK4yTmQaTAStP5qoszNHoNZt2hyJqqKMBTCSx5Q
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0.006396
    Expected counter: 54734
    Gas limit: 60238
    Storage limit: 0 bytes
    Balance updates:
      tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ............ -ꜩ0.006396
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,83) ... +ꜩ0.006396
    Transaction:
      Amount: ꜩ0
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      To: KT1UvdRG22BE96VJMfECHn5uEtjGFWsKV1BU
      Parameter: (Pair "edsigtm7116sRJH8S3PW8N2TxJinC9cP3LmEKn7DZ1wJzQxaqHo6gZq9cCvRgTJzHKA2XxoqnkJjZMjkefPMk523Hn5dhhLUZJQ"
                       (Pair 0 (Left (Left 7))))
      This transaction was successfully applied
      Updated storage:
        (Pair 1
              (Pair 0x00cc80ab168b04973d9e1f9d4d2248b077a9250d3bce750b2735b4818a7b9bb7d3
                    0x01e50b3dd46e79dad935179dd9326a4e495598769600))
      Storage size: 681 bytes
      Consumed gas: 41816
    Internal operations:
      Transaction:
        Amount: ꜩ0
        From: KT1UvdRG22BE96VJMfECHn5uEtjGFWsKV1BU
        To: KT1VTqmma3vCH9nkLL1Jakd6MiUwxwqieXDE
        Parameter: (Right (Left 7))
        This transaction was successfully applied
        Updated storage:
          (Pair 7 0x01df2408d0767b08295b38e2fc0d49bee0cf7652ee00)
        Storage size: 355 bytes
        Consumed gas: 18322
```

Confirm the value is stored in the oracle contract:

```bash
❯❯❯ alpha-client get contract storage for $ORACLE_ADDRESS

Pair 7 "KT1UvdRG22BE96VJMfECHn5uEtjGFWsKV1BU"
```

