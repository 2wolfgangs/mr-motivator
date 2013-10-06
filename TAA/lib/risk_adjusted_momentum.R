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
  x <- as.double(index.xts(equity))
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