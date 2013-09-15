# ================================================================
# Prepare data for SIT backtesting and store in 'data' environment
# ================================================================
data <- new.env()

# Convert reuters downloads to xts objects
# ----------------------------------------
data$commodity <- xts(commodity.reuters[,-1], order.by=dmy(commodity.reuters[,1]))
data$intl.reit <- xts(intlreit.reuters[,-1], order.by=dmy(intlreit.reuters[,1]))

# Clean up Quandl gold data
# -------------------------
# Get USD data only
data$gold <- raw.quandl.data$gold[,1]
# And rename column name to Close
names(data$gold) <- 'Close'

# Clean up yahoo data
# -------------------------------
# Adjust OHLC data
adjusted.yahoo.data <- new.env()
for(i in ls(raw.yahoo.data)) {
  adjusted.yahoo.data[[i]] <- adjustOHLC(raw.yahoo.data[[i]], use.Adjusted=T)
}

# Give the yahoo data nice names and copy into data environment
data$us.eq       <- adjusted.yahoo.data$SPY
data$int.eq      <- adjusted.yahoo.data$VGTSX
data$japan.eq    <- adjusted.yahoo.data$EWJ
data$em.eq       <- adjusted.yahoo.data$VEIEX
data$us.reit     <- adjusted.yahoo.data$VGSIX
data$int.treas   <- adjusted.yahoo.data$VFIUX
data$long.treas  <- adjusted.yahoo.data$VUSTX
data$short.treas <- adjusted.yahoo.data$VFSIX

# Remove days when the market was not open
# ----------------------------------------
# Some reuters data has history on days the US stock market was not open.
# This causes problems when calculating moving averages etc. so we want to remove it.
# SPY has the largest history, we'll use that to get the trading days.
market.days <- index(data$us.eq)

# And subset all time series by these days
for (i in ls(data)) {
  data[[i]] <- data[[i]][market.days] 
}

# Prepare the SIT backtesting environment using bt.prep function
# --------------------------------------------------------------
bt.prep(data, align='keep.all', dates='1993:12::')

# In aligning all the data, we've now introduced some NAs into the data. 
# We're going to carry the last value forward, to remove the NAs.
for (i in ls(data)) {
  data[[i]] <- na.locf(data[[i]]) 
}
 