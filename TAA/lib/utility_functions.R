#*****************************************************************
# UTILITY FUNCTIONS
# ================================
#*****************************************************************
# For these functions to run effectively, the environment must 
# contain: 
#  - data          (an SIT backtest environment)
#  - period.ends   (list of rebalance period indices to be applied to prices)  
#
# Uses some code from https://github.com/systematicinvestor/SIT
#*****************************************************************


#******************************************************************
# Create standardised momentum measure over a range of lookback periods
#******************************************************************
# Args: 
#   momentumfunc - Momentum function e.g. function(x) calculateZScore(x)
#   momlookback  - vector containing momentum lookbacks
#
# Returns:
#   xts containing cross sectional momentums, standardised over a 
#   range of lookback periods
#******************************************************************
standardiseMomentumOverLookbacks <- function(momentumfunc, momlookback) {
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
runEqualWeightBacktest <- function (
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
runComboBacktest <- function () {
  
  data$weight[] <- NA
  # Normalise weights to sum to 1
  data$weight <- accumulated.weight / rowSums(accumulated.weight[period.ends[length(period.ends)]])
  
  return(bt.run.share(data, clean.signal=F, trade.summary=T))
}
