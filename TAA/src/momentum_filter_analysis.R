#*****************************************************************
# Momentum Analysis with Gaussian Filter
# Uses code from https://github.com/systematicinvestor/SIT
#*****************************************************************
library('ProjectTemplate')
load.project()

# Set version of data to run over
data <- data.3
  
# Reset variables in case we've run before.
models <- list()
if (exists('accumulated.momentum')) rm(accumulated.momentum)
if (exists('accumulated.weight')) rm(accumulated.weight)

# Momentum lookback (1,3,6,9,12 months)
mom.lookbacks <- c(1*22,3*22,6*22,9*22,12*22) 
slowfastratio <- 10

# Standardisation method
std.type <- 'zdist'

# Get xts of combined prices from data environment
price.data <- data$prices  
n <- ncol(price.data)

# Find period ends using xts endpoints function
period.ends <- endpoints(price.data, 'months')
period.ends <- period.ends[period.ends > 0]

#*****************************************************************
# Total return momentum 
#*****************************************************************

totalreturn <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateTRMomentum(x,y)
                                                , price.data, mom.lookbacks)

models$totalreturn <- runMomentumWeightedBacktest(totalreturn,0,0,TRUE)

#*****************************************************************
# Total return less most recent month
#******************************************************************
# trx1 <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateTRx1Momentum(x,y) 
#                                         , price.data, mom.lookbacks) 

# models$trx <- runMomentumWeightedBacktest(trx1,0,0.8,FALSE)

#*****************************************************************
# SMA differential
#*****************************************************************
smadiff <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateSMADifferential(x,y)
                                            , price.data, mom.lookbacks) 

models$smadiff <- runMomentumWeightedBacktest(smadiff,0,0,TRUE)

#*****************************************************************
# Price to SMA differential
#*****************************************************************
pricesmadiff <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculatePriceToSMADifferential(x,y)
                                                 , price.data, mom.lookbacks) 

models$pricesmadiff <- runMomentumWeightedBacktest(pricesmadiff,0,0,TRUE)

#*****************************************************************
# Instantaneous Slope
# Daily rate of change of SMA 
#*****************************************************************
instslope <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateInstantaneousSlope(x,y)
                                              , price.data, mom.lookbacks) 

models$instslope <- runMomentumWeightedBacktest(instslope,0,0,TRUE)

#*****************************************************************
# Percentile Rank
# See: http://en.wikipedia.org/wiki/Percentile_rank
#*****************************************************************
# percentrank <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculatePercentileRank(x,y)
#                                                , price.data, mom.lookbacks) 

# models$percentrank <- runMomentumWeightedBacktest(percentrank,0,0.8,FALSE)

#*****************************************************************
# Z score
# The magnitude that the current price deviates
# from the average price over the period
#*****************************************************************
# zscore <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateZScore(x,y)
#                                           , price.data, mom.lookbacks) 

# models$zscore <- runMomentumWeightedBacktest(zscore,0,0.8,FALSE)

#*****************************************************************
# Z distribution 
# A transformation of the z score to a percentile value on the 
# cumulative normal distribution
#*****************************************************************
# zdist <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateZDistribution(x,y)
#                                          , price.data, mom.lookbacks) 

# models$zdist <- runMomentumWeightedBacktest(zdist,0,0.8,FALSE)

#*****************************************************************
# SharpeRatio
#*****************************************************************

sharpe <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateSharpeRAM(x,y)
                                           , price.data, mom.lookbacks)

models$sharpe <- runMomentumWeightedBacktest(sharpe,0,0,TRUE)

#*****************************************************************
# Omega Ratio
#******************************************************************
omega <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateOmegaRAM(x,y) 
                                          , price.data, mom.lookbacks) 

models$omega <- runMomentumWeightedBacktest(omega,0,0,TRUE)


#*****************************************************************
# Equal weighted portfolio backtest
# Potfolio of all models, equal weighted.
#*****************************************************************
models$combo <- runComboBacktest()

#*****************************************************************
# Create benchmark porfolios
#*****************************************************************
benchmarks <- runBenchmarkPortfolios()


#*****************************************************************
# Create Report
#******************************************************************    
models = rev(models)

strategy.performance.snapshoot(models, T)
strategy.performance.snapshoot(benchmarks, T)
 