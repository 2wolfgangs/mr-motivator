#*****************************************************************
# RISK ADJUSTED MOMENTUM FUNCTIONS
# ================================
#*****************************************************************
# Makes use of PerformanceAnalytics package
# http://cran.r-project.org/web/packages/PerformanceAnalytics/PerformanceAnalytics.pdf
#*****************************************************************
require(PerformanceAnalytics)
require(TTR)

#*****************************************************************
# Sharpe Ratio 
#*****************************************************************
calculateSharpeRAM <- function(prices, n.mom) {
  rtn <- CalculateReturns(prices)
  mean <- applyFunctionToMatrix(rtn, function(x) { runMean(x, n.mom) })
  std <- applyFunctionToMatrix(rtn, function(x) { runSD(x, n.mom) } ) 
  return(mean/std)
}

#*****************************************************************
# Omega Ratio 
#*****************************************************************
calculateOmegaRAM <- function(prices, n.mom) {
  rtn <- CalculateReturns(prices)
  positive.returns <- rtn * (rtn > 0)
  negative.returns <- (0 - rtn) * (rtn < 0)
  positive.sum <- applyFunctionToMatrix(positive.returns, function(x) { runSum(x, n.mom) } )
  negative.sum <- applyFunctionToMatrix(negative.returns, function(x) { runSum(x, n.mom) } )
  return (positive.sum / negative.sum)
}

#*****************************************************************
# Sortino ratio
#*****************************************************************
calculateSortinoRAM <- function(prices, n.mom) {
  rtn <- CalculateReturns(prices)
  sortino <- applyTSFunctionToMatrix(rtn, function(x) {
    apply.rolling(x, width=n.mom, FUN="SortinoRatio") 
  }) 
  return (sortino)
}

#*****************************************************************
# Calmar ratio
#*****************************************************************
calculateCalmarRAM <- function(prices, n.mom) {
  rtn <- CalculateReturns(prices)
  
  calmar <- applyTSFunctionToMatrix(rtn, function(x) {
    apply.rolling(x, width=n.mom, FUN="CalmarRatio") 
  })   
  return (calmar)
}

#*****************************************************************
# DVR ratio
# This is the product of the sharpe ratio and the R-squared of the
# equity curve
#*****************************************************************
calculateDVRRAM <- function(prices, n.mom) {
  sharpe <- calculateSharpeRAM(prices, n.mom)
  equity.curves <- getEquityCurveFromPrices(prices)
  x <- as.double(index.xts(equity.curves))
  y <- equity.curves
  rsquared <- applyFunctionToMatrix(y, function(y) {
    runCov(y,x,n.mom)
  })
  return(sharpe * rsquared)
}

#*****************************************************************
# Value at Risk
# This is the return over Value at Risk.
# Effectively, this is the sharpe ratio, with VaR as the denominator
#*****************************************************************
calculateVaRRAM <- function(prices, n.mom) {
  rtn <- CalculateReturns(prices)
  var <- applyTSFunctionToMatrix(rtn, function(x) {
    apply.rolling(x, width=n.mom, FUN="calculateSharpeVaR") 
  }) 
  return (var)
}

#*****************************************************************
# Conditional Value at Risk
# This is the return over Value at Risk.
# Effectively, this is the sharpe ratio, with ES (cVaR) as the denominator
#*****************************************************************
calculateCVaRRAM <- function(prices, n.mom) {
  rtn <- CalculateReturns(prices)
  var <- applyTSFunctionToMatrix(rtn, function(x) {
    apply.rolling(x, width=n.mom, FUN="calculateSharpeCVaR") 
  }) 
  return (var)
}

#*****************************************************************
# Return to max loss
# This is the return over the minimum daily return
#*****************************************************************
calculateMaxLossRAM <- function(prices, n.mom) {
  rtn <- CalculateReturns(prices)
  mean <- applyFunctionToMatrix(rtn, function(x) { runMean(x, n.mom) })
  maxloss <- applyFunctionToMatrix(rtn, function(x) { runMin(x, n.mom) })   
  return (mean / abs(maxloss))
}

#*****************************************************************
# Return to average drawdown
# This is the return over the average drawdown 
#*****************************************************************
calculateMaxLossRAM <- function(prices, n.mom) {
  rtn <- CalculateReturns(prices)
  mean <- applyFunctionToMatrix(rtn, function(x) { runMean(x, n.mom) })
  average.drawdown <- applyTSFunctionToMatrix(rtn, function(x) {
    apply.rolling(x, width=n.mom, FUN="AverageDrawdown") 
  })    
  return (mean / average.drawdown)
}

# TODO: High-Low Differential 

#*****************************************************************
# Ulcer index
# From PerformanceAnalytics package documentation...
# "This is similar to drawdown deviation except that the impact of the 
#  duration of drawdowns is incorporated by selecting the negative
#  return for each period below the previous peak or high water mark. 
#  The impact of long, deep drawdowns will have significant impact 
#  because the underperformance since the last peak is squared."
#*****************************************************************
calculateUlcerRAM <- function(prices, n.mom) {
  rtn <- CalculateReturns(prices)
  ulcer <- applyTSFunctionToMatrix(rtn, function(x) {
    apply.rolling(x, width=n.mom, FUN="UlcerIndex") 
  })    
  return (ulcer)
}

#*****************************************************************
# Gain to Pain Ratio
# This is the sum of returns over the absolute value of the sum of
# losses
#*****************************************************************
calculateGainToPainRAM <- function(prices, n.mom) {
  rtn <- CalculateReturns(prices)
  negative.returns <- (0 - rtn) * (rtn < 0)
  return.sum <- applyFunctionToMatrix(rtn, function(x) { runSum(x, n.mom) } )
  negative.sum <- applyFunctionToMatrix(negative.returns, function(x) { runSum(x, n.mom) } )
  return (return.sum / negative.sum)
}

#*****************************************************************
# UTILITY FUNCTIONS
#*****************************************************************

#******************************************************************
# Get Sharpe Ratio with denominator as VAR
#******************************************************************
calculateSharpeVaR <- function(price.data) {
  return(SharpeRatio(price.data, FUN="VaR"))
}

#******************************************************************
# Get Sharpe Ratio with denominator as cVAR
#******************************************************************
calculateSharpeCVaR <- function(price.data) {
  return(SharpeRatio(price.data, FUN="ES"))
}

