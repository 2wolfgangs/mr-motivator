#*****************************************************************
# STANDARDISATION FUNCTIONS
# =========================
#*****************************************************************
# Uses code from https://github.com/systematicinvestor/SIT
#*****************************************************************

#******************************************************************
# Create standardised momentum measure over a range of lookback periods
#******************************************************************
# Args: 
#   momentumfunc - Momentum function e.g. function(x) calculateZScore(x)
#   price.data   - time series object containing price data
#   momlookback  - vector containing momentum lookbacks
#
# Returns:
#   xts containing cross sectional momentums, standardised over a 
#   range of lookback periods
#******************************************************************
standardiseMomentumOverLookbacks <- function(std.method = 'abs', momentumfunc, price.data, momlookback) {
  stdmom <- 0 
  for (look in momlookback[1:length(momlookback)]) {
    moms <- momentumfunc(price.data, look)
    
    # Standardise according to specified method
    if (std.method == 'abs') {
      stmomentum <- standardiseAbsoluteValue(moms)  
    }
    if (std.method == 'zscore') {
      stmomentum <- standardiseZScore(moms)
    }
    if (std.method == 'zdist') {
      stmomentum <- standardiseZDistribution(moms)
    }
    
    # Keep a running total accross lookback periods
    stdmom <- stdmom + stmomentum
  }  
  # Return standardised momentum amount, normalised to 1 or -1
  return(stdmom / length(momlookback))
}

#******************************************************************
# Standardise using Absolute Value Releveraging method
#******************************************************************
# See: http://gestaltu.com/2013/05/dynamic-asset-allocation-for-practitioners-part-1-the-many-faces-of-momentum-2.html
#
# Args:
#   input.momentum - time series object containing values for all assets
#
# Returns:
#   xts containing standardised values
#******************************************************************
standardiseAbsoluteValue <- function(input.momentum) {
  # abs(asset momentum) / abs(sum of all asset momentums) * sign of asset momentum
  return ((abs(input.momentum) / rowSums(abs(input.momentum), na.rm = TRUE)) 
          * sign(input.momentum))
}

#******************************************************************
# Standardise using Z Score
#******************************************************************
# Z Score = Price - Mean(x) / Stdev(x)
#
# Args:
#   input.momentum - time series object containing values for all assets
#
# Returns:
#   xts containing standardised values
#******************************************************************
standardiseZScore <- function(input.momentum) {
  return((input.momentum - rowMeans(input.momentum, na.rm = TRUE)) 
         / apply(input.momentum, 1, function(x) {sd(x, na.rm = TRUE)}))
}

#******************************************************************
# Standardise using Z Distribution
#******************************************************************
# Z Distribution = pnorm(Price - Mean(x) / Stdev(x))
#
# Args:
#   input.momentum - time series object containing values for all assets
#
# Returns:
#   xts containing standardised values
#******************************************************************
standardiseZDistribution <- function(input.momentum) {
  return(pnorm(standardiseZScore(input.momentum)))
}

