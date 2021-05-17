# Your Oanda API Key
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
FROM_DATE = '2020-01-01T15:00:00.000000000Z'

# format: '2016-10-17T15:00:00.000000000Z'
# leave None for present time
TO_DATE = '2020-05-01T15:00:00.000000000Z'

# (1 - 5000) must be in quotes e.g. '100'
# leave None if using dates, else fill with number of past candlesticks you want up to present time
COUNT = None

# 'practice' or 'trade'
ACCOUNT_TYPE = 'practice'


# DO NOT EDIT
PARAMS = {
    'from': FROM_DATE,
    'to': TO_DATE,
    'count': COUNT,
    'granularity': GRANULARITY
}
