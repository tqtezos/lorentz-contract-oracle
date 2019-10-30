
## Originating the contract

```bash
❯❯❯ stack build

❯❯❯ stack exec -- basic-exe --help
Lorentz tools

Usage: basic-exe [--version] COMMAND
  Sale contract parameter generation helper

Available options:
  -h,--help                Show this help text
  --version                Show version.

Available commands:
  print                    Dump the sale contract in form of Michelson code
  print-tunnel             Dump the tunnel contract in form of Michelson code
  init                     Initial storage for the sale contract
  init-allowance           Initial storage for the allowance tunnel contract
  init-balance             Initial storage for the balance tunnel contract
  purchase                 purchase
  update-price             update price
  get-price                get price
  get-held                 get price
  get-wanted               get price
  get-wallet               get price
  set-tunnels              set proxies

You can use help for specific COMMAND
EXAMPLE:
  lorentz-contract-sale ManagedLedger --help
```

Sale contract code:

```
❯❯❯ stack exec -- basic-exe print --oneline
```

Sale contract initial storage:
- `held` and `wanted` are FA1.2.1 addresses
- `wallet` is the wallet holding the FA1.2.1 tokens to trade
- The price is: sell a multiple of `held-price` `held` tokens for the corresponding multiple of `wanted-price` `wanted` tokens:
  e.g. price is `(held: 2, wanted: 3)` so you could buy `2` for `3`, `4` for `6`, `6` for `9`, etc.

```bash
❯❯❯ stack exec -- basic-exe init --help
Usage: basic-exe init --admin ADDRESS --held ADDRESS --wallet ADDRESS
                      --wanted ADDRESS --held-price NATURAL
                      --wanted-price NATURAL
  Initial storage for the sale contract

Available options:
  -h,--help                Show this help text
  --admin ADDRESS          Address of the admin.
  --held ADDRESS           Address of the held.
  --wallet ADDRESS         Address of the wallet.
  --wanted ADDRESS         Address of the wanted.
  --held-price NATURAL     Natural number representing held-price.
  --wanted-price NATURAL   Natural number representing wanted-price.
```

Next, originate the two tunnels:

Tunnel code

```
❯❯❯ stack exec -- basic-exe print-tunnel --oneline
```

Initial storage for allowance (held token) tunnel:
- `held` is held token contract address
- `sale` is Sale contract address

```
❯❯❯ stack exec -- basic-exe init-allowance --help
Usage: basic-exe init-allowance --held ADDRESS --sale ADDRESS
  Initial storage for the allowance tunnel contract

Available options:
  -h,--help                Show this help text
  --held ADDRESS           Address of the held.
  --sale ADDRESS           Address of the sale.
```

Initial storage for allowance (held token) tunnel:
- `wanted` is wanted token contract address
- `sale` is Sale contract address

```bash
❯❯❯ stack exec -- basic-exe init-balance --help
Usage: basic-exe init-balance --wanted ADDRESS --sale ADDRESS
  Initial storage for the balance tunnel contract

Available options:
  -h,--help                Show this help text
  --wanted ADDRESS         Address of the wanted.
  --sale ADDRESS           Address of the sale.
```

After they're originated and you have their addresses,
set the tunnel addresses in the sale contract by calling
the parameter generated by `set-tunnels`:
- `held` is the `allowance` or `held` token tunnel contract address
- `wanted` is the `balance` or `wanted` token tunnel contract address

```
❯❯❯ stack exec -- basic-exe set-tunnels --help
Usage: basic-exe set-tunnels --held ADDRESS --wanted ADDRESS
  set proxies

Available options:
  -h,--help                Show this help text
  --held ADDRESS           Address of the held.
  --wanted ADDRESS         Address of the wanted.
```


## Beginning a sale

To begin a sale, simply `setAllowance` on the held-token wallet
to however many tokens you'd like to sell.

The sale begins instantly once the allowance is set (to a non-zero number).


## Performing a purchase

The buyer will likely want to verify the held/wanted token contract addresses
as well as the current price before continuing.

There are `View` parameters (`get-..`) for these values, but you can check
offline with `alpha-client get contract storage for $ADDRESS`

```bash
❯❯❯ stack exec -- basic-exe get-price --help
Usage: basic-exe get-price --callback ADDRESS
  get price

Available options:
  -h,--help                Show this help text
  --callback ADDRESS       Address of the callback.
```


Next, the buyer transfers a multiple of the `wanted-price` number
of `wanted` tokens to the contract address (not the wallet address).


Once the tokens are transferred, you can call purchase to complete the purchase:
- `held-price` and `wanted-price` should match the price stored in the contract exactly

```bash
❯❯❯ stack exec -- basic-exe purchase --help
Usage: basic-exe purchase --held-price NATURAL --wanted-price NATURAL
  purchase

Available options:
  -h,--help                Show this help text
  --held-price NATURAL     Natural number representing held-price.
  --wanted-price NATURAL   Natural number representing wanted-price.
```

As long as:
- There are sufficiently many tokens in the wallet (that holds the sale's tokens)
- The number of tokens traded is an exact multiple of the `wanted-price`

The purchase will complete, performing the following steps:
  1. Perform and wait for allowance callback (to find how many tokens can be accessed from wallet)
  2. Perform and wait for balance on self address (to find out how many tokens trasferred by investor)
  3. Run validation, if it fails transfer balance back to caller
  4. Transfer tokens from self address to wallet
  5. Transfer tokens from wallet to buyer
