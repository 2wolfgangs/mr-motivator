# ================================================================
# Prepare data for SIT backtesting and store in 'data.5' environment
# ================================================================
data.5 <- new.env()

for(i in ls(raw.yahoo.data.nasdaq)) {
  data.5[[i]] = adjustOHLC(raw.yahoo.data.nasdaq[[i]], use.Adjusted=T)  
}

for (i in ls(data.5)) {
  data.5[[i]] <- na.locf(data.5[[i]]) 
}

bt.prep(data.5, align='keep.all', dates='1995::')
