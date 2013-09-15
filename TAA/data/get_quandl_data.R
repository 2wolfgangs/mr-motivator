# =====================================================
# Get Quandl data
# =====================================================

# London PM Fix Price for Gold 
# Date Range: 1993-01-01 to 2013-08-31

raw.quandl.data <- new.env()

Quandl.auth('iJKuQxCzwusUxG6o4Dwn')

raw.quandl.data$gold <- Quandl('OFDP/GOLD_2', type = 'xts', start_date = '1993-01-01', 
                               end_date = '2013-08-30')


