 #*****************************************************************
# MOMENTUM FUNCTIONS
# ==================
#*****************************************************************
# For these functions to run effectively, the environment must 
# contain: 
#  - data        (an SIT backtest environment)
#  - prices      (xts - daily prices)
#  - period.ends (list of rebalance period indices to be applied to prices)    
#
# Uses some code from https://github.com/systematicinvestor/SIT
#*****************************************************************

require(TTR)

#*****************************************************************
# Total return momentum 
#*****************************************************************
TRMomentum <- function(n.mom) {
  return((prices / mlag(prices, n.mom)) - 1)
}

#*****************************************************************
# Total return less most recent month
#*****************************************************************
TRx1Momentum <- function(n.mom) {
  return((mlag(prices, 22) / mlag(prices, n.mom + 22)) - 1)
}  

#*****************************************************************
# SMA differential
#*****************************************************************
SMADifferential <- function(n.mom) {
  smafast <- bt.apply(data, function(x) { SMA(Cl(x), n.mom / slowfastratio) } )
  smaslow <- bt.apply(data, function(x) { SMA(Cl(x), n.mom) } ) 
  return((smafast / smaslow) - 1)
}

#*****************************************************************
# Price to SMA differential
#*****************************************************************
PriceToSMADifferential <- function(n.mom) {
  smaslow <- bt.apply(data, function(x) { SMA(Cl(x), n.mom) } ) 
  return((prices / smaslow) - 1)
}

#*****************************************************************
# Instantaneous Slope
# Daily rate of change of SMA 
#*****************************************************************
InstantaneousSlope <- function(n.mom) {
  smaslow <- bt.apply(data, function(x) { SMA(Cl(x), n.mom) } )
  return((smaslow / mlag(smaslow, 1) - 1))
}

#*****************************************************************
# Percentile Rank
# See: http://en.wikipedia.org/wiki/Percentile_rank
#*****************************************************************
PercentileRank <- function(n.mom) {
  return(bt.apply(data, function(x) { runPercentRank(Cl(x), n.mom) } ))
}

#*****************************************************************
# Z score
# The magnitude that the current price deviates
# from the average price over the period
#*****************************************************************
ZScore <- function(n.mom) {
  zmean <- bt.apply(data, function(x) { runMean(Cl(x), n.mom) } )
  zsd <- bt.apply(data, function(x) { runSD(Cl(x), n.mom) } )
  return((prices - zmean) / zsd)
}

#*****************************************************************
# Z distribution 
# A transformation of the z score to a percentile value on the 
# cumulative normal distribution
#*****************************************************************
ZDistribution <- function(n.mom) {
  zmean <- bt.apply(data, function(x) { runMean(Cl(x), n.mom) } )
  zsd <- bt.apply(data, function(x) { runSD(Cl(x), n.mom) } )
  return(pnorm((prices - zmean) / zsd))
}


#******************************************************************
# UTILITY FUNCTIONS
# =================
#******************************************************************

#******************************************************************
# Create standardised momentum measure over a range of lookback periods
#******************************************************************
# Args: 
#   momentumfunc - Momentum function
#
# Returns:
#   xts containing cross sectional momentums, standardised over a 
#   range of lookback periods
#******************************************************************
StandardisedMomentum <- function(momentumfunc) {
  stdmom <- 0 
  for (look in momlookback[1:length(momlookback)]) {
    moms <- momentumfunc(look)
    # Standardised momentum
    # abs(asset momentum) / abs(sum of all asset momentums) * sign of asset momentum
    stmomentum <- (abs(moms) / abs(rowSums(moms, na.rm = TRUE))) * sign(moms)
    stdmom <- stdmom + stmomentum
  }  
  # Return standardised momentum amount, normalised to 1 or -1
  return(stdmom / length(momlookback))
}

#******************************************************************
# Run Equal Weight Backtest with n.positions
#******************************************************************
#
# Args:
#   momentum.matrix - xts object containing momentums for each asset
#                     such as those returned from StandardisedMomentum()
#   n.positions     - number of positions to hold (int) 
#   accumulate      - set to TRUE to accumulate weights in global 
#                     accumulated.weight object. Default is FALSE.
#
# Returns:
#   SIT backtest model environment
#   
#******************************************************************
RunEqualWeightBacktest <- function (
    momentum.matrix, 
    n.positions, 
    accumulate = FALSE
  ) {
  
  data$weight[] <- NA
  data$weight[period.ends,] <- ntop(momentum.matrix[period.ends,], n.positions)
  
  if(accumulate) {
    if(!exists('accumulated.weight')) {
      accumulated.weight <<- data$weight
    } else {
      accumulated.weight <<- accumulated.weight + data$weight
    }
  }
  
  return(bt.run.share(data, clean.signal=F, trade.summary=T))
}

#******************************************************************
# Run Backtest for equal weighted combo portfolio
#******************************************************************
#
# Normalised the weights in accumulated.weight to sum to zero
# and run backtest.
#
# Returns:
#   SIT backtest model environment
#   
#******************************************************************
RunComboBacktest <- function () {
  
  data$weight[] <- NA
  # Normalise weights to sum to 1
  data$weight <- accumulated.weight / rowSums(accumulated.weight[period.ends[length(period.ends)]])
  
  return(bt.run.share(data, clean.signal=F, trade.summary=T))
}
