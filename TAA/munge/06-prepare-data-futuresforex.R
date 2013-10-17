# ================================================================
# Prepare data for SIT backtesting and store in 'data.5' environment
# ================================================================
data.6 <- new.env()

data.6 <- raw.tb.data

for (i in ls(data.6)) {
  data.6[[i]] <- na.locf(data.6[[i]]) 
}

bt.prep(data.6, align='remove.na', dates='1990 ::')
