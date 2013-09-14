# ================================================================
# Prepare data for SIT backtesting and store in 'data' environment
# ================================================================
data <- new.env()

# Convert reuters downloads to xts objects
# ----------------------------------------
data$commodity.old <- xts(commodity.reuters[,-1], order.by=dmy(commodity.reuters[,1]))
data$intlreit.old <- xts(intlreit.reuters[,-1], order.by=dmy(intlreit.reuters[,1]))
data$usreit.old <- xts(usreit.reuters[,-1], order.by=dmy(usreit.reuters[,1])) 

# Clean up Quandl gold data
# -------------------------
# Get USD data only
# data$gold.old <- raw.quandl.data$gold[,1]
# And rename to Close
# names(data$gold.old) <- 'Close'

# Adjust OHLC data for yahoo data
# -------------------------------
adjusted.yahoo.data <- new.env()
for(i in ls(raw.yahoo.data)) {
  adjusted.yahoo.data[[i]] <- adjustOHLC(raw.yahoo.data[[i]], use.Adjusted=T)
}

# Give the yahoo data nice names and copy into data environment
data$commodity   <- adjusted.yahoo.data$DBC
data$gold        <- adjusted.yahoo.data$GLD
data$us.eq       <- adjusted.yahoo.data$SPY
data$int.eq      <- adjusted.yahoo.data$VGTSX
data$japan.eq    <- adjusted.yahoo.data$EWJ
data$em.eq       <- adjusted.yahoo.data$VEIEX
data$int.reit    <- adjusted.yahoo.data$RWX
data$us.reit     <- adjusted.yahoo.data$VGSIX
data$int.treas   <- adjusted.yahoo.data$VFIUX
data$long.treas  <- adjusted.yahoo.data$VUSTX
data$short.treas <- adjusted.yahoo.data$VFSIX

# Prepare the SIT backtesting environment
bt.prep(data, align='keep.all', dates='1993:12::')

 