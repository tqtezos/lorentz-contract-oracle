from alpha_vantage.timeseries import TimeSeries
from apscheduler.schedulers.background import BackgroundScheduler
from datetime import datetime, timezone
from flask import Flask
from pytezos import pytezos, Key
import atexit, base64, os, tempfile

oracle_address = os.environ['ORACLE_ADDRESS']
alpha_vantage_api_key = os.environ['ALPHA_VANTAGE_API_KEY']
alpha_vantage_ticker_symbol = os.environ['ALPHA_VANTAGE_TICKER_SYMBOL']

tezos_user_key = None
with tempfile.NamedTemporaryFile(suffix='.json', delete=False) as key_file:
    key_file.write(base64.standard_b64decode(os.environ['TEZOS_USER_KEY']))
    key_file.close()
    tezos_user_key = Key.from_faucet(key_file.name)
    os.remove(key_file.name)

class OracleServer:
    def __init__(self, api_key=alpha_vantage_api_key, ticker_symbol=alpha_vantage_ticker_symbol, tezos_key=tezos_user_key, oracle_contract_address=oracle_address):
        print('__init__')
        self.oracle_contract_address = oracle_contract_address
        self.pytezos_instance = pytezos.using(key=tezos_key, shell='babylonnet')
        self.time_series = TimeSeries(key=api_key, output_format='json')
        self.ticker_symbol = ticker_symbol

    def oracle_contract(self):
        print(self.pytezos_instance)
        print(self.pytezos_instance.contract('KT1DUT68M5Hn8Fu1GkiQ6KVCep6BDMwueFMk'))
        return self.pytezos_instance.contract(self.oracle_contract_address)

    def update_value(self):
        try:
            now_utc = datetime.now(tz=timezone.utc)
            print(now_utc)
            operation_group = self.oracle_contract().update_value(value_timestamp=now_utc, value=self.price()).operation_group
            operation_str = f"<p> Last operation:\n{operation_group.autofill().sign().inject()} </p>"
            storage_str = f"<p> Current storage:\n{self.oracle_contract().storage()} </p>"
            return (operation_str + storage_str)
        except Exception as e:
            raise e

#             exception_doc = f"<p> Exception: {str(e.__doc__)} </p>"
#             exception_message = None
#             try:
#                 exception_message = f"<p> {str(e.message)} </p>"
#             except:
#                 exception_message = f"(unknown message: {e.__class__.__name__})"
#             return (exception_doc + exception_message)

    def price(self):
        quote_data, expected_none = self.time_series.get_quote_endpoint(self.ticker_symbol)
        if expected_none is None:
            try:
                return int(float(quote_data['05. price']) * 100)
            except KeyError:
                raise f"expected key: '05. price' in {str(quote_data)}"
        else:
            raise f"expected_none not None: {str(expected_none)}"

def update_oracle():
    print('called update_oracle')
    result_str = str(OracleServer().update_value())
    print('')
    print(result_str)
    print('')
    return result_str

app = Flask(__name__)

@app.route('/')
def index():
    return update_oracle(), 200

oracle_update_scheduler = BackgroundScheduler()
oracle_update_scheduler.add_job(func=update_oracle, trigger="interval", seconds=30)
oracle_update_scheduler.start()
atexit.register(lambda: oracle_update_scheduler.shutdown())

