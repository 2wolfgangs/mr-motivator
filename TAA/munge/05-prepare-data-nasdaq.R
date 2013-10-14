# ================================================================
# Prepare data for SIT backtesting and store in 'data.5' environment
# ================================================================
data.5 <- new.env()

for(i in ls(raw.yahoo.data.nasdaq)) {
  data.5[[i]] = adjustOHLC(raw.yahoo.data.nasdaq[[i]], use.Adjusted=T)  
}

bt.prep(data.5, align='keep.all', dates='1995::')
