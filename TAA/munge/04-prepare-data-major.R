# ================================================================
# Prepare data for SIT backtesting and store in 'data.4' environment
# ================================================================
data.4 <- new.env()

for(i in ls(raw.yahoo.data.major)) {
  data.4[[i]] = adjustOHLC(raw.yahoo.data.major[[i]], use.Adjusted=T)  
}

for (i in ls(data.4)) {
  data.4[[i]] <- na.locf(data.4[[i]]) 
}

bt.prep(data.4, align='keep.all', dates='2003:10::')
