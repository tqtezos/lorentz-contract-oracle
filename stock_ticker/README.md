
NOTE: This is currently a WIP of being updated for `Carthage`

# Intro

You can find a deployed copy of the oracle contract that receives
`AAPL` stock prices every ~30 seconds [here](https://better-call.dev/carthage/KT1CUTjTqf4UMf6c9A8ZA4e1ntWbLVTnvrKG/operations)

# Setup

## On OSX:

```bash
❯❯❯ brew tap cuber/homebrew-libsecp256k1
❯❯❯ brew install libsodium libsecp256k1 gmp
```
Note: for the following, order matters: if you run the `PIP_IGNORE_INSTALLED=1`
after `pytezos`, it won't recognize the system-installed `libsecp256k1` and
will fail attempting to install the Python-bundled version.

```bash
❯❯❯ pipenv --three --site-packages    
❯❯❯ PIP_IGNORE_INSTALLED=1 pipenv install --dev alpha-vantage APScheduler Flask 
❯❯❯ pipenv install pytezos
```

To run locally:

```bash
TEZOS_USER_KEY=".." \
ORACLE_ADDRESS="KT1CUTjTqf4UMf6c9A8ZA4e1ntWbLVTnvrKG" \
ALPHA_VANTAGE_API_KEY=".." \
ALPHA_VANTAGE_TICKER_SYMBOL="AAPL" \
FLASK_APP="tq/oracles/ticker.py" \
pipenv run -- flask run --host 0.0.0.0
```


## Setup Cont.

To update `requirements.txt`:

```bash
pipenv lock -r > requirements.txt
```

NOTE: See `Dockerfile` for exceptions: `pipenv` doesn't
include all requirments in `Pipfile.lock`.

To run the Flask app:

```bash
FLASK_APP="tq/oracles/ticker.py" TEZOS_USER_KEY="$(base64 ~/Downloads/tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr.json | tr -d '\n')" ORACLE_ADDRESS="KT1CUTjTqf4UMf6c9A8ZA4e1ntWbLVTnvrKG" ALPHA_VANTAGE_API_KEY=".." ALPHA_VANTAGE_TICKER_SYMBOL="AAPL" flask run
```

To build the `Docker` image:

```bash
docker build -t oracle-stock-ticker:latest .
```

To push it to DockerHub:

```bash
docker images
docker tag 68861b674784 tqtezos/oracle-stock-ticker:latest 
docker push tqtezos/oracle-stock-ticker
```

To run:

```bash
docker run -d -p 5000:5000 --env TEZOS_USER_KEY="$(base64 ~/Downloads/tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr.json | tr -d '\n')" --env ORACLE_ADDRESS="KT1CUTjTqf4UMf6c9A8ZA4e1ntWbLVTnvrKG" --env ALPHA_VANTAGE_API_KEY=".." --env ALPHA_VANTAGE_TICKER_SYMBOL="AAPL" oracle-stock-ticker
```

Build and run:

```bash
❯❯❯ docker build -t oracle-stock-ticker:latest . && docker run -d -p 5000:5000 \
  --env TEZOS_USER_KEY=".." \
  --env ORACLE_ADDRESS="KT1CUTjTqf4UMf6c9A8ZA4e1ntWbLVTnvrKG" \
  --env ALPHA_VANTAGE_API_KEY=".." \
  --env ALPHA_VANTAGE_TICKER_SYMBOL="AAPL" \
  --env FLASK_APP="tq/oracles/ticker.py" \
  oracle-stock-ticker:latest
```

All environment variables:

- To generate the `TEZOS_USER_KEY` parameter, run: `echo "$(base64 MY_KEY_FILE.json | tr -d '\n')"`,
  where `MY_KEY_FILE.json` is your Tezos faucet file (see [here](https://faucet.tzalpha.net/) to get a testnet faucet file).
- Alpha Vantage provides free API keys for stock ticker prices: https://www.alphavantage.co

```bash
TEZOS_USER_KEY=".."
ORACLE_ADDRESS="KT1CUTjTqf4UMf6c9A8ZA4e1ntWbLVTnvrKG"
ALPHA_VANTAGE_API_KEY=".."
ALPHA_VANTAGE_TICKER_SYMBOL="AAPL"
FLASK_APP="tq/oracles/ticker.py"
```

