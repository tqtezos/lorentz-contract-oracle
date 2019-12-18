
To update `requirements.txt`:

```bash
pip3 freeze >| requirements.txt 
```

To run the Flask app:

```bash
FLASK_APP="tq/oracles/ticker.py" TEZOS_USER_KEY="$(base64 ~/Downloads/tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr.json | tr -d '\n')" ORACLE_ADDRESS="KT1EGbAxguaWQFkV3Egb2Z1r933MWuEYyrJS" ALPHA_VANTAGE_API_KEY=".." ALPHA_VANTAGE_TICKER_SYMBOL="AAPL" flask run
```

To build the `Docker` image:

```bash
docker build -t oracle-stock-ticker:latest .
```

To run:

```bash
docker run -d -p 5000:5000 --env TEZOS_USER_KEY="$(base64 ~/Downloads/tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr.json | tr -d '\n')" --env ORACLE_ADDRESS="KT1EGbAxguaWQFkV3Egb2Z1r933MWuEYyrJS" --env ALPHA_VANTAGE_API_KEY=".." --env ALPHA_VANTAGE_TICKER_SYMBOL="AAPL" oracle-stock-ticker
```

Build and run:

```bash
❯❯❯ docker build -t oracle-stock-ticker:latest . && docker run --rm -it -p 5000:5000 --env TEZOS_USER_KEY="$(base64 ~/Downloads/tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr.json | tr -d '\n')" --env ORACLE_ADDRESS="KT1EGbAxguaWQFkV3Egb2Z1r933MWuEYyrJS" --env ALPHA_VANTAGE_API_KEY=".." --env ALPHA_VANTAGE_TICKER_SYMBOL="AAPL" --env FLASK_APP="tq/oracles/ticker.py" oracle-stock-ticker
```

All environment variables:

- To generate the `TEZOS_USER_KEY` parameter, run: `echo "$(base64 MY_KEY_FILE.json | tr -d '\n')"`,
  where `MY_KEY_FILE.json` is your Tezos faucet file (see [here](https://faucet.tzalpha.net/) to get a testnet faucet file).
- Alpha Vantage provides free API keys for stock ticker prices: https://www.alphavantage.co

```bash
TEZOS_USER_KEY=".."
ORACLE_ADDRESS="KT1EGbAxguaWQFkV3Egb2Z1r933MWuEYyrJS"
ALPHA_VANTAGE_API_KEY=".."
ALPHA_VANTAGE_TICKER_SYMBOL="AAPL"
FLASK_APP="tq/oracles/ticker.py"
```

