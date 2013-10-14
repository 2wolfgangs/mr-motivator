# ================================================================
# Prepare data for SIT backtesting and store in 'data.5' environment
# ================================================================
data.6 <- new.env()

data.6 <- raw.tb.data
bt.prep(data.6, align='remove.na', dates='1990 ::')
