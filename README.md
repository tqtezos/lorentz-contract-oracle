
See [`quick_start.md`](quick_start.md)

# The CLI

```bash
❯❯❯ ./stack exec -- lorentz-contract-oracle Oracle --help
Usage: lorentz-contract-oracle Oracle COMMAND
  Oracle contract CLI interface

Available options:
  -h,--help                Show this help text

Available commands:
  print                    Dump the Oracle contract in form of Michelson code
  print-timestamped        Dump the Timestamped Oracle contract in form of
                           Michelson code
  init                     Initial storage for the Oracle contract
  get-value                get value
  update-value             update value
  update-admin             update admin
```


# Originating the contract


## Printing the contract

The print command takes a single argument: `valueType`, the type of the value
provided by the oracle.

For example, if `nat`'s are provided:

```bash
❯❯❯ ./stack exec -- lorentz-contract-oracle Oracle print --valueType "nat"
parameter (or (pair unit
                    (contract nat))
              (or nat
                  address));
storage (pair nat
              address);
code { DUP;
       CAR;
       DIP { CDR };
       IF_LEFT { DUP;
                 CAR;
                 DIP { CDR };
                 DIP { DIP { DUP };
                       SWAP };
                 PAIR;
                 CDR;
                 CAR;
                 DIP { AMOUNT };
                 TRANSFER_TOKENS;
                 NIL operation;
                 SWAP;
                 CONS;
                 PAIR }
               { IF_LEFT { DIP { DUP;
                                 CAR;
                                 DIP { CDR } };
                           DIP { DROP;
                                 DUP;
                                 DIP { SENDER;
                                       COMPARE;
                                       EQ;
                                       IF {  }
                                          { PUSH string "only admin may update";
                                            FAILWITH } } };
                           PAIR;
                           NIL operation;
                           PAIR }
                         { DIP { DUP;
                                 CAR;
                                 DIP { CDR };
                                 DIP { SENDER;
                                       COMPARE;
                                       EQ;
                                       IF {  }
                                          { PUSH string "only admin may update";
                                            FAILWITH } } };
                           SWAP;
                           PAIR;
                           NIL operation;
                           PAIR } } };
```

## Initial storage

```bash
❯❯❯ ./stack exec -- lorentz-contract-oracle Oracle init --help
Usage: lorentz-contract-oracle Oracle init --initialValueType Michelson Type
                                           --initialValue Michelson Value
                                           --admin ADDRESS
  Initial storage for the Oracle contract

Available options:
  -h,--help                Show this help text
  --initialValueType Michelson Type
                           The Michelson Type of initialValue
  --initialValue Michelson Value
                           The Michelson Value: initialValue
  --admin ADDRESS          Address of the admin.
  -h,--help                Show this help text
```

Since we're using `nat`, `initialValueType` is `nat` and `initialValue` can be `0`:

```bash
❯❯❯ ./stack exec -- lorentz-contract-oracle Oracle init \
  --initialValueType "nat" \
  --initialValue 3 \
  --admin $ALICE_ADDRESS
Pair 3 "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr"
```

## Running the origination

```bash
alpha-client --wait none originate contract NatOracle \
  transferring 0 from $ALICE_ADDRESS running \
  "$(./stack exec -- lorentz-contract-oracle Oracle print --valueType "nat")" \
  --init "$(./stack exec -- lorentz-contract-oracle Oracle init \
  --initialValueType "nat" --initialValue 3 \
  --admin $ALICE_ADDRESS)" --burn-cap 0.612

Waiting for the node to be bootstrapped before injection...
Current head: BKvCNwrrSzp7 (timestamp: 2019-12-07T00:30:08-00:00, validation: 2019-12-07T00:30:46-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 18987 units (will add 100 for safety)
Estimated storage: 612 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'onrarWezTWeZmBZxNM5edtYirv8ZdECxmjpvVRj8tA2JEaYNQJC'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for onrarWezTWeZmBZxNM5edtYirv8ZdECxmjpvVRj8tA2JEaYNQJC to be included --confirmations 30 --branch BKvCNwrrSzp7nav3iYQCC37pnsqDMMrFTWw3Z2nPNiHiRAT2LAh
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0.002508
    Expected counter: 30709
    Gas limit: 19087
    Storage limit: 632 bytes
    Balance updates:
      tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ............ -ꜩ0.002508
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,59) ... +ꜩ0.002508
    Origination:
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      Credit: ꜩ0
      Script:
        { parameter (or (pair unit (contract nat)) (or nat address)) ;
          storage (pair nat address) ;
          code { DUP ;
                 ...
                         PAIR } } } }
        Initial storage: (Pair 3 "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr")
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1VTqmma3vCH9nkLL1Jakd6MiUwxwqieXDE
        Storage size: 355 bytes
        Paid storage size diff: 355 bytes
        Consumed gas: 18987
        Balance updates:
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.355
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.257

New contract KT1VTqmma3vCH9nkLL1Jakd6MiUwxwqieXDE originated.
Contract memorized as NatOracle.
```

```bash
❯❯❯ ORACLE_ADDRESS="KT1VTqmma3vCH9nkLL1Jakd6MiUwxwqieXDE"
```

# Getting a value

## Preparing a view contract

Originate the contract:

```bash
❯❯❯ alpha-client --wait none originate contract nat_storage transferring 0 \
  from $ALICE_ADDRESS running "$(lorentz-contract print --name NatStorageContract)" \
  --init 0 --burn-cap 0.295
```

Make an alias for its address:

```bash
❯❯❯ NAT_STORAGE_ADDRESS="KT1JDLPVp9trdzNB7Xk1ETVXjaGDTMENn1vk"
```

See the [`FA1.2` Quickstart](https://assets.tqtezos.com/quickstart/3-index) for more info.


## Make the parameter

```bash
❯❯❯ ./stack exec -- lorentz-contract-oracle Oracle get-value --help
Usage: lorentz-contract-oracle Oracle get-value --callbackContract ADDRESS
  get value

Available options:
  -h,--help                Show this help text
  --callbackContract ADDRESS
                           Address of the callbackContract.
  -h,--help                Show this help text
```

```bash
❯❯❯ ./stack exec -- lorentz-contract-oracle Oracle get-value --callbackContract $NAT_STORAGE_ADDRESS
Left (Pair Unit "KT1JDLPVp9trdzNB7Xk1ETVXjaGDTMENn1vk")
```

## Get the value

```bash
❯❯❯ alpha-client --wait none transfer 0 from $ALICE_ADDRESS to $ORACLE_ADDRESS \
  --arg "$(./stack exec -- lorentz-contract-oracle Oracle get-value \
  --callbackContract $NAT_STORAGE_ADDRESS)" --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BMeYNKNuMDGK (timestamp: 2019-12-07T00:38:10-00:00, validation: 2019-12-07T00:39:04-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 30424 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'onhfCx9f5khcQbmtermQpidTTKeJ9p6xLJN2C1sCtMcUxtxj2Jt'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for onhfCx9f5khcQbmtermQpidTTKeJ9p6xLJN2C1sCtMcUxtxj2Jt to be included --confirmations 30 --branch BMeYNKNuMDGKdyYH1VkUo3oPxYhKx812EEStVpD9wpSTCWtKU97
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0.003356
    Expected counter: 30711
    Gas limit: 30524
    Storage limit: 0 bytes
    Balance updates:
      tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ............ -ꜩ0.003356
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,59) ... +ꜩ0.003356
    Transaction:
      Amount: ꜩ0
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      To: KT1VTqmma3vCH9nkLL1Jakd6MiUwxwqieXDE
      Parameter: (Left (Pair Unit "KT1JDLPVp9trdzNB7Xk1ETVXjaGDTMENn1vk"))
      This transaction was successfully applied
      Updated storage:
        (Pair 3 0x00003b5d4596c032347b72fb51f688c45200d0cb50db)
      Storage size: 355 bytes
      Consumed gas: 19100
    Internal operations:
      Transaction:
        Amount: ꜩ0
        From: KT1VTqmma3vCH9nkLL1Jakd6MiUwxwqieXDE
        To: KT1JDLPVp9trdzNB7Xk1ETVXjaGDTMENn1vk
        Parameter: 3
        This transaction was successfully applied
        Updated storage: 3
        Storage size: 38 bytes
        Consumed gas: 11324
```

# Update the value

## Make the parameter

```bash
❯❯❯ ./stack exec -- lorentz-contract-oracle Oracle update-value --help
Usage: lorentz-contract-oracle Oracle update-value --newValueType Michelson Type
                                                   --newValue Michelson Value
  update value

Available options:
  -h,--help                Show this help text
  --newValueType Michelson Type
                           The Michelson Type of newValue
  --newValue Michelson Value
                           The Michelson Value: newValue
  -h,--help                Show this help text
```

## Update the value

~/C/m/lorentz-contract-oracle ❯❯❯ ./stack exec -- lorentz-contract-oracle Oracle update-value --newValueType "nat" --newValue 4    


