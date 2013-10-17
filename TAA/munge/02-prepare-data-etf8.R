# ================================================================
# Prepare data for SIT backtesting and store in 'data.2' environment
# ================================================================
data.2 <- new.env()

for(i in ls(raw.yahoo.data.8ETF)) {
  data.2[[i]] <- adjustOHLC(raw.yahoo.data.8ETF[[i]], use.Adjusted=T)  
}

for (i in ls(data.2)) {
  data.2[[i]] <- na.locf(data.2[[i]]) 
}

# TLT first date is 7/31/2002
bt.prep(data.2, align='keep.all', dates='2002:08::') 


 