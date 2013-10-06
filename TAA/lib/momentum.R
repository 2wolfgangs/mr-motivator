#*****************************************************************
# MOMENTUM FUNCTIONS
# ==================
#*****************************************************************
# Uses code from https://github.com/systematicinvestor/SIT
# Uses TTR http://cran.r-project.org/web/packages/TTR/index.html
#*****************************************************************

require(TTR,SIT)


#*****************************************************************
# Total return momentum 
#*****************************************************************
calculateTRMomentum <- function(prices, n.mom) {
  return((prices / lagMatrix(prices, n.mom)) - 1)
}

#*****************************************************************
# Total return less most recent month
#*****************************************************************
calculateTRx1Momentum <- function(prices, n.mom) {
  return((lagMatrix(prices, 22) / lagMatrix(prices, n.mom + 22)) - 1)
}  

#*****************************************************************
# SMA differential
#*****************************************************************
calculateSMADifferential <- function(prices, n.mom) {
  
  # If slowfastratio is not specified in global environment, set to 10
  if (!exists('slowfastratio')) {
    slowfastratio <- 10
  }
  
  smafast <- applyFunctionToMatrix(prices, function(x) { SMA(x, n.mom / slowfastratio) } )
  smaslow <- applyFunctionToMatrix(prices, function(x) { SMA(x, n.mom) } ) 
  return((smafast / smaslow) - 1)
}

#*****************************************************************
# Price to SMA differential
#*****************************************************************
calculatePriceToSMADifferential <- function(prices, n.mom) {
  smaslow <- applyFunctionToMatrix(prices, function(x) { SMA(x, n.mom) } ) 
  return((prices / smaslow) - 1)
}

#*****************************************************************
# Instantaneous Slope
# Daily rate of change of SMA 
#*****************************************************************
calculateInstantaneousSlope <- function(prices, n.mom) {
  smaslow <- applyFunctionToMatrix(prices, function(x) { SMA(x, n.mom) } )
  return((smaslow / lagMatrix(smaslow, 1) - 1))
}

#*****************************************************************
# Percentile Rank
# See: http://en.wikipedia.org/wiki/Percentile_rank
#*****************************************************************
calculatePercentileRank <- function(prices, n.mom) {
  return(applyFunctionToMatrix(prices, function(x) { runPercentRank(x, n.mom) } ))
}

#*****************************************************************
# Z score
# The magnitude that the current price deviates
# from the average price over the period
#*****************************************************************
calculateZScore <- function(prices, n.mom) {
  zmean <- applyFunctionToMatrix(prices, function(x) { runMean(x, n.mom) } )
  zsd <- applyFunctionToMatrix(prices, function(x) { runSD(x, n.mom) } )
  return((prices - zmean) / zsd)
}

#*****************************************************************
# Z distribution 
# A transformation of the z score to a percentile value on the 
# cumulative normal distribution
#*****************************************************************
calculateZDistribution <- function(prices, n.mom) {
  zmean <- applyFunctionToMatrix(prices, function(x) { runMean(x, n.mom) } )
  zsd <- applyFunctionToMatrix(prices, function(x) { runSD(x, n.mom) } )
  return(pnorm((prices - zmean) / zsd))
}


