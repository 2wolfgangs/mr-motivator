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
# Run Equal Weight Backtest with n.positions
#******************************************************************
#
# Args:
#   momentum.matrix - xts object containing momentums for each asset
#                     such as those returned from standardiseMomentumOverLookbacks()
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
# Apply function to a matrix (usually xts)
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
  