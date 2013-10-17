# ================================================================
# Prepare data for SIT backtesting and store in 'data.3' environment
# ================================================================
data.3 <- new.env()

for(i in ls(raw.yahoo.data.dow)) {
  data.3[[i]] = adjustOHLC(raw.yahoo.data.dow[[i]], use.Adjusted=T)  
}

for (i in ls(data.3)) {
  data.3[[i]] <- na.locf(data.3[[i]]) 
}

bt.prep(data.3, align='keep.all', dates='1980::') 


 