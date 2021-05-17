import pandas as pd
import requests

API_KEY = 'Bearer e3920e0f60c88136aa66dca3bbfbae7f-bb9c32f4d0ab0fc0106137b0467a4394'

HEADERS = {
    'Content-Type': 'application/json',
    'Authorization': API_KEY
}

# format: Currency1_Currency2
INSTRUMENT = 'EUR_USD'

# e.g. 'S5', 'M15', 'H1', 'D', 'W', 'M'
# https://developer.oanda.com/rest-live-v20/instrument-df/ for full options
GRANULARITY = 'M30'

# format: '2016-06-01T15:00:00.000000000Z'
# leave None if using COUNT
#FROM_DATE = '2020-01-01T15:00:00.000000000Z'

# format: '2016-10-17T15:00:00.000000000Z'
# leave None for present time
#TO_DATE = '2020-05-01T15:00:00.000000000Z'

# (1 - 5000) must be in quotes e.g. '100'
# leave None if using dates, else fill with number of past candlesticks you want up to present time
COUNT = None

# 'practice' or 'trade'
ACCOUNT_TYPE = 'practice'


def export_ba_data():
    try:
        r = requests.get("https://api-fx{}.oanda.com/v3/instruments/{}/candles?price=BA".format(
            ACCOUNT_TYPE, INSTRUMENT), headers=HEADERS, params=PARAMS).json()

        df = pd.DataFrame(([candle['time'], candle['ask']['o'], candle['ask']['h'], candle['ask']['l'], candle['ask']['c'], candle['volume']] for candle in r['candles']),
                          columns=['Time', 'Open', 'High', 'Low', 'Close', 'Volume'])
        print(FROM_DATE)
        print(TO_DATE)
        # df.to_csv('ask_data.csv', index=False)
        return df
    except:
        print('request failed, likely that candlestick count is out of range (max=50000)')


#####################################
from_year = 2020
from_month = 1
to_year = 2021
to_month = 1
#####################################
first_time = 1

for y in range(from_year, to_year+1):
    if ((y == to_year) and (y == from_year)):
        m_range_min = from_month
        m_range_max = to_month-1
    elif(y == from_year):
        m_range_min = from_month
        m_range_max = 12
    elif(y == to_year):
        m_range_min = 1
        m_range_max = to_month-1
    else:
        m_range_min = 1
        m_range_max = 12

    for m in range(m_range_min, m_range_max+1):
        m1 = m
        m2 = m+1
        if(m1 >= 10):
            FROM_DATE = str(y)+'-'+str(m1)+'-01T15:00:00.000000000Z'
        else:
            FROM_DATE = str(y)+'-0'+str(m1)+'-01T15:00:00.000000000Z'
        if(m2 >= 10):
            if(m2 == 13):
                TO_DATE = str(y+1)+'-0'+str(1)+'-01T15:00:00.000000000Z'
            else:
                TO_DATE = str(y)+'-'+str(m2)+'-01T15:00:00.000000000Z'
        else:
            TO_DATE = str(y)+'-0'+str(m2)+'-01T15:00:00.000000000Z'
            #TO_DATE = '2020-05-01T15:00:00.000000000Z'
        PARAMS = {
            'from': FROM_DATE,
            'to': TO_DATE,
            'count': COUNT,
            'granularity': GRANULARITY
        }
        # print(FROM_DATE)
        # print(TO_DATE)
        if(first_time):
            df = export_ba_data()
            first_time = 0
        else:
            df_aux = export_ba_data()
            df = df.append(df_aux)

df.to_csv('ask_data.csv', index=False)
print('bid/ask data exported to "ask_data.csv"')
