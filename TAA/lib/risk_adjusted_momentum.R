#*****************************************************************
# RISK ADJUSTED MOMENTUM FUNCTIONS
# ================================
#*****************************************************************
# For these functions to run effectively, the environment must 
# contain: 
#  - data          (an SIT backtest environment)
#  - prices        (xts - daily prices)
#  - period.ends   (list of rebalance period indices to be applied to prices)  
#
# Uses some code from https://github.com/systematicinvestor/SIT
#*****************************************************************

require(TTR)

#*****************************************************************
# Sharpe Ratio 
#*****************************************************************
calculateSharpeRAM <- function(n.mom) {
  rtn <- ((prices / mlag(prices, n.mom)) - 1)
  std <- apply(prices, 2, function(x) { runSD(x, n.mom) } )
  return(rtn / std)
}

#*****************************************************************
# Omega Ratio 
#*****************************************************************
calculateOmegaRAM <- function(n.mom) {
  daily.returns <- ((prices / mlag(prices,1)) - 1) 
  positive.returns <- daily.returns * (daily.returns > 0)
  negative.returns <- daily.returns * (daily.returns < 0)
  positive.sum <- apply(positive.returns, 2, function(x) { runSum(x, n.mom) } )
  negative.sum <- apply(negative.returns, 2, function(x) { runSum(x, n.mom) } )
  return (positive.sum / negative.sum)
  
}

#*****************************************************************
# Sortino ratio
#*****************************************************************
