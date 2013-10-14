# ================================================================
# Prepare data for SIT backtesting and store in 'data.1' environment
# ================================================================
data.1 <- new.env()

# Convert reuters downloads to xts objects
# ----------------------------------------
data.1$commodity <- xts(commodity.reuters[,-1], order.by=dmy(commodity.reuters[,1]))
data.1$intl.reit <- xts(intlreit.reuters[,-1], order.by=dmy(intlreit.reuters[,1]))

# Clean up Quandl gold data.1
# -------------------------
# Get USD data.1 only
data.1$gold <- raw.quandl.data$gold[,1]
# And rename column name to Close
names(data.1$gold) <- 'Close'

# Clean up yahoo data.1
# -------------------------------
# Adjust OHLC data.1
adjusted.yahoo.data <- new.env()
for(i in ls(raw.yahoo.data)) {
  adjusted.yahoo.data[[i]] <- adjustOHLC(raw.yahoo.data[[i]], use.Adjusted=T)
}

# Give the yahoo data.1 nice names and copy into data.1 environment
data.1$us.eq       <- adjusted.yahoo.data$SPY
data.1$int.eq      <- adjusted.yahoo.data$VGTSX
data.1$japan.eq    <- adjusted.yahoo.data$EWJ
data.1$em.eq       <- adjusted.yahoo.data$VEIEX
data.1$us.reit     <- adjusted.yahoo.data$VGSIX
data.1$int.treas   <- adjusted.yahoo.data$VFIUX
data.1$long.treas  <- adjusted.yahoo.data$VUSTX
data.1$short.treas <- adjusted.yahoo.data$VFSIX

# Remove days when the market was not open
# ----------------------------------------
# Some reuters data.1 has history on days the US stock market was not open.
# This causes problems when calculating moving averages etc. so we want to remove it.
# SPY has the largest history, we'll use that to get the trading days.
market.days <- index(data.1$us.eq)

# And subset all time series by these days
for (i in ls(data.1)) {
  data.1[[i]] <- data.1[[i]][market.days] 
}

# Prepare the SIT backtesting environment using bt.prep function
# --------------------------------------------------------------
bt.prep(data.1, align='keep.all', dates='1994:12::')

# In aligning all the data.1, we've now introduced some NAs into the data.1. 
# We're going to carry the last value forward, to remove the NAs.
for (i in ls(data.1)) {
  data.1[[i]] <- na.locf(data.1[[i]]) 
}
