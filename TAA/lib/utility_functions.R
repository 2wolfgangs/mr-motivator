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
require(SIT)
require(PerformanceAnalytics)

#******************************************************************
# Run Equal Weight Backtest with n.positions
#******************************************************************
#
# Args:
#   momentum.matrix - xts object containing momentums for each asset
#                     such as those returned from standardiseMomentumOverLookbacks()
#   n.positions     - max number of positions to hold. Default is 0 (no restrictions) 
#   filter.threshold- threshold for gaussian-normalised momentum score.
#                     Default is 0 (no filter)
#   accumulate      - set to TRUE to accumulate weights in global 
#                     accumulated.weight object. Default is FALSE.
#
# Returns:
#   SIT backtest model environment
#   
#******************************************************************
runEqualWeightBacktest <- function (
  momentum.matrix, 
  n.positions = 0, 
  filter.threshold = 0,
  accumulate = FALSE
) {
  
  # If we've passed in 0 at the number of positions, we want to equal weight all assets.
  # So set n.positions to the number of columns in momentum.matrix
  if (n.positions == 0) {
    n.positions <- ncol(momentum.matrix)
  }
  
  # Equal weight top n assets at the period.end re-balancing points
  data$weight[] <- NA
  data$weight[period.ends,] <- ntop(momentum.matrix[period.ends,], n.positions)
  
  # Apply filter if we have one
  if (filter.threshold != 0) {
    normalised.momentum <- standardiseMomentumAcrossAssets(momentum.matrix)
    filtered.mom <- normalised.momentum > filter.threshold
    # Set weights to zero where the normalised momentum is below filter value
    data$weight <- data$weight * filtered.mom
    # Now releverage the weights to 1
    data$weight <- data$weight / rowSums(data$weight, na.rm = TRUE)
  }
          
  if(accumulate) {
    if(!exists('accumulated.weight')) {
      accumulated.weight <<- data$weight
    } else {
      accumulated.weight <<- accumulated.weight + data$weight
    }
  }
  
  return(bt.run.share(data, clean.signal=F, trade.summary=F))
}

#******************************************************************
# Run Momenutm Weighted Backtest with n.positions
#******************************************************************
#
# Args:
#   momentum.matrix - xts object containing momentums for each asset
#                     such as those returned from standardiseMomentumOverLookbacks()
#   n.positions     - max number of positions to hold. Default is 0 (no restrictions) 
#   filter.threshold- threshold for gaussian-normalised momentum score.
#                     Default is 0 (no filter)
#   accumulate      - set to TRUE to accumulate weights in global 
#                     accumulated.weight object. Default is FALSE.
#
# Returns:
#   SIT backtest model environment
#   
#******************************************************************
runMomentumWeightedBacktest <- function (
  momentum.matrix, 
  n.positions = 0, 
  filter.threshold = 0,
  accumulate = FALSE
) {
  
  # If we've passed in 0 at the number of positions, we want to equal weight all assets.
  # So set n.positions to the number of columns in momentum.matrix
  if (n.positions == 0) {
    n.positions <- ncol(momentum.matrix)
  }
  
  # Equal weight top n assets at the period.end re-balancing points
  data$weight[] <- NA
  data$weight[period.ends,] <- ntop(momentum.matrix[period.ends,], n.positions)
  
  # Apply filter if we have one
  if (filter.threshold != 0) {
    normalised.momentum <- standardiseMomentumAcrossAssets(momentum.matrix)
    filtered.mom <- normalised.momentum > filter.threshold
    # Set weights to zero where the normalised momentum is below filter value
    data$weight <- data$weight * filtered.mom * normalised.momentum
    # Now releverage the weights to 1
    data$weight <- data$weight / rowSums(data$weight, na.rm = TRUE)
  }
  
  if(accumulate) {
    if(!exists('accumulated.weight')) {
      accumulated.weight <<- data$weight
    } else {
      accumulated.weight <<- accumulated.weight + data$weight
    }
  }
  
  return(bt.run.share(data, clean.signal=F, trade.summary=F))
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

#******************************************************************
# Run Benchmark Portfolios
#******************************************************************
#
# Run backtest for equal weight and min correlation benchmark portfolios
#
# Returns:
#   SIT backtest model environment
#   
#******************************************************************
runBenchmarkPortfolios <- function (rebalanceperiod = 'months') {
  
  obj <- portfolio.allocation.helper(  data$prices, periodicity = rebalanceperiod,
                                       min.risk.fns = list(EW=equal.weight.portfolio,
                                                        RP=risk.parity.portfolio,
                                                        MV=min.var.portfolio,
                                                        MD=max.div.portfolio,
                                                        MC=min.corr.portfolio,
                                                        MC2=min.corr2.portfolio),
                                       custom.stats.fn = 'portfolio.allocation.custom.stats'
                                    ) 
  
  benchmark.models <- create.strategies(obj, data)$models
  
  return(benchmark.models)
}

#******************************************************************
# Apply function to xts matrix 
#******************************************************************
#
# Applies a function to a matrix.
# (Mostly copied from bt.apply.matrix in SIT package)
#
# Args:
#   matrix   - The matrix to apply the function to (usually xts)
#   apply.function  -Function to apply
#   ....     - Other parameters
#
# Returns:
#   the results of applying the function to the matrix
#   
#******************************************************************
applyFunctionToMatrix <- function(matrix, apply.function, ...) {
  result <- matrix
  result[] <- NA
  n.cols <- ncol(matrix)
  
  for( i in 1:n.cols) {
    msg <- try( match.fun(apply.function)( coredata(matrix[,i]),... ) , silent=TRUE)
    if (class(msg)[1] != 'try-error') {
      result[,i] = msg
    } else {
      cat(i, msg, '\n')
    }
  }
  return(result)  
}

#******************************************************************
# Apply time series function to xts matrix 
#******************************************************************
#
# Applies a timeseries function to a matrix. Used instead of
# applyFunctionToMatrix for PerformanceAnalytics functions.
# This does some hackery to line up the result set nicely.
# (Mostly copied from bt.apply.matrix in SIT package)
#
# Args:
#   matrix   - The matrix to apply the function to (usually xts)
#   apply.function  -Function to apply
#   ....     - Other parameters
#
# Returns:
#   the results of applying the function to the matrix
#   
#******************************************************************
applyTSFunctionToMatrix <- function(matrix, apply.function, ...) {
  result <- matrix
  result[] <- NA
  n.cols <- ncol(matrix)
  n.rows <- nrow(matrix)
  
  for( i in 1:n.cols) {
    msg <- try( match.fun(apply.function)( matrix[,i],... ) , silent=TRUE)
    if (class(msg)[1] != 'try-error') {
      # We assume that we only have NAs at the start of the price series
      # So we use the number of rows in the result vector to calculate an
      # offset by which we can line up the data.
      # TODO: There's surely a better way of doing this...
      result.rows <- nrow(msg)
      offset <- n.rows - result.rows + 1
      result[offset:n.rows,i] <- msg
    } else {
      cat(i, msg, '\n')
    }
  }
  return(result)  
}
     
#******************************************************************
# Lag matrix 
# (Copied from mlag in SIT package)
#******************************************************************
#
# Moves elements in a matrix by a given lag value.
# lagMatrix(m,1) will use yesterday's values
#
# Args:
#   matrix   - The matrix to lag
#   lag      - Number of values to lag by
#
# Returns:
#   lagged matrix
#   
#******************************************************************
lagMatrix <- function (matrix, lag) {
  if( is.null(dim(matrix)) ) { 
    lag <- len(matrix)
    if(nlag > 0) {
      matrix[(lag+1):n] <- matrix[1:(n-lag)]
      matrix[1:lag] <- NA
    } else if(lag < 0) {
      matrix[1:(n+lag)] <- matrix[(1-lag):n]
      matrix[(n+lag+1):n] <- NA
    } 	
    
  } else {
    n <- nrow(matrix)
    if(lag > 0) {
      matrix[(lag+1):n,] <- matrix[1:(n-lag),]
      matrix[1:lag,] <- NA
    } else if(lag < 0) {
      matrix[1:(n+lag),] <- matrix[(1-lag):n,]
      matrix[(n+lag+1):n,] <- NA
    } 
  }
  return(matrix);
}

#******************************************************************
# Get equity curve from price data
#******************************************************************
#
# Returns normalised equity curves (with equity starting at 1)
# for a price series
#
# Args:
#   prices   - xts matrix of prices
#   
# Returns:
#   matrix of equity curves
#   
#******************************************************************
getEquityCurveFromPrices <- function(price.data) {
  gross.returns <- CalculateReturns(price.data) + 1
  gross.returns[1,] <- 1
  equity <- cumprod(gross.returns)
  return(equity)
}

