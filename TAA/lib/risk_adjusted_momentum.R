#*****************************************************************
# RISK ADJUSTED MOMENTUM FUNCTIONS
# ================================
#*****************************************************************
# Makes heavy use of PerformanceAnalytics package
# http://cran.r-project.org/web/packages/PerformanceAnalytics/PerformanceAnalytics.pdf
#*****************************************************************

#*****************************************************************
# Sharpe Ratio 
#*****************************************************************
calculateSharpeRAM <- function(prices, n.mom) {
  rtn <- CalculateReturns(prices)
  std <- applyFunctionToMatrix(prices, function(x) { runSD(x, n.mom) } )
  return(rtn / std)
}

#*****************************************************************
# Omega Ratio 
#*****************************************************************
calculateOmegaRAM <- function(prices, n.mom) {
  daily.returns <- ((prices / mlag(prices,1)) - 1) 
  positive.returns <- daily.returns * (daily.returns > 0)
  negative.returns <- daily.returns * (daily.returns < 0)
  positive.sum <- applyFunctionToMatrix(positive.returns, function(x) { runSum(x, n.mom) } )
  negative.sum <- applyFunctionToMAtrix(negative.returns, function(x) { runSum(x, n.mom) } )
  return (positive.sum / negative.sum)
}

#*****************************************************************
# Sortino ratio
#*****************************************************************
