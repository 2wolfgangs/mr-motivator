# =====================================================
# Gets CSI futures and forex data from tradingblox website
# =====================================================
#
# For data set 6 (Futures and Forex)

# Put all the yahoo data in its own environment,
# so we can munge it separately and more efficiently.
raw.tb.data <- new.env()

# Read data into yahoodata environment using quantmod
getSymbols.TB(env = raw.tb.data, auto.assign = TRUE, download = TRUE) 


