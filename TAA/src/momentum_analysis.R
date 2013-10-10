#*****************************************************************
# Raw Momentum Analysis
# Uses code from https://github.com/systematicinvestor/SIT
#*****************************************************************
library('ProjectTemplate')
load.project()

# Reset variables in case we've run before.
models <- list()
accumulated.momentum <- NA

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

models$totalreturn2 <- runEqualWeightBacktest(totalreturn,2,0,TRUE)
models$totalreturn3 <- runEqualWeightBacktest(totalreturn,3,0,TRUE)
models$totalreturn4 <- runEqualWeightBacktest(totalreturn,4,0,TRUE)
models$totalreturn5 <- runEqualWeightBacktest(totalreturn,5,0,FALSE)

#*****************************************************************
# Total return less most recent month
#******************************************************************
trx1 <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateTRx1Momentum(x,y) 
                                         , price.data, mom.lookbacks) 

models$trx2 <- runEqualWeightBacktest(trx1,2,0,FALSE)
models$trx3 <- runEqualWeightBacktest(trx1,3,0,FALSE)
models$trx4 <- runEqualWeightBacktest(trx1,4,0,FALSE)
models$trx5 <- runEqualWeightBacktest(trx1,5,0,FALSE)

#*****************************************************************
# SMA differential
#*****************************************************************
smadiff <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateSMADifferential(x,y)
                                            , price.data, mom.lookbacks) 

models$smadiff2 <- runEqualWeightBacktest(smadiff,2,0,TRUE)
models$smadiff3 <- runEqualWeightBacktest(smadiff,3,0,TRUE)
models$smadiff4 <- runEqualWeightBacktest(smadiff,4,0,TRUE)
models$smadiff5 <- runEqualWeightBacktest(smadiff,5,0,FALSE)

#*****************************************************************
# Price to SMA differential
#*****************************************************************
pricesmadiff <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculatePriceToSMADifferential(x,y)
                                                 , price.data, mom.lookbacks) 

models$pricesmadiff2 <- runEqualWeightBacktest(pricesmadiff,2,0,TRUE)
models$pricesmadiff3 <- runEqualWeightBacktest(pricesmadiff,3,0,TRUE)
models$pricesmadiff4 <- runEqualWeightBacktest(pricesmadiff,4,0,TRUE)
models$pricesmadiff5 <- runEqualWeightBacktest(pricesmadiff,5,0,FALSE)

#*****************************************************************
# Instantaneous Slope
# Daily rate of change of SMA 
#*****************************************************************
instslope <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateInstantaneousSlope(x,y)
                                              , price.data, mom.lookbacks) 

models$instslope2 <- runEqualWeightBacktest(instslope,2,0,TRUE)
models$instslope3 <- runEqualWeightBacktest(instslope,3,0,TRUE)
models$instslope4 <- runEqualWeightBacktest(instslope,4,0,TRUE)
models$instslope5 <- runEqualWeightBacktest(instslope,5,0,FALSE)

#*****************************************************************
# Percentile Rank
# See: http://en.wikipedia.org/wiki/Percentile_rank
#*****************************************************************
percentrank <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculatePercentileRank(x,y)
                                                , price.data, mom.lookbacks) 

models$percentrank2 <- runEqualWeightBacktest(percentrank,2,0,FALSE)
models$percentrank3 <- runEqualWeightBacktest(percentrank,3,0FALSE)
models$percentrank4 <- runEqualWeightBacktest(percentrank,4,0,FALSE)
models$percentrank5 <- runEqualWeightBacktest(percentrank,5,0,FALSE)

#*****************************************************************
# Z score
# The magnitude that the current price deviates
# from the average price over the period
#*****************************************************************
zscore <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateZScore(x,y)
                                           , price.data, mom.lookbacks) 

models$zscore2 <- runEqualWeightBacktest(zscore,2,0,FALSE)
models$zscore3 <- runEqualWeightBacktest(zscore,3,0,FALSE)
models$zscore4 <- runEqualWeightBacktest(zscore,4,0,FALSE)
models$zscore5 <- runEqualWeightBacktest(zscore,5,0,FALSE)

#*****************************************************************
# Z distribution 
# A transformation of the z score to a percentile value on the 
# cumulative normal distribution
#*****************************************************************
zdist <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateZDistribution(x,y)
                                          , price.data, mom.lookbacks) 

models$zdist2 <- runEqualWeightBacktest(zdist,2,0,FALSE)
models$zdist3 <- runEqualWeightBacktest(zdist,3,0,FALSE)
models$zdist4 <- runEqualWeightBacktest(zdist,4,0,FALSE)
models$zdist5 <- runEqualWeightBacktest(zdist,5,0,FALSE)

#*****************************************************************
# Equal weighted portfolio backtest
# Potfolio of all models, equal weighted.
#*****************************************************************
models$combo <- runComboBacktest()

#*****************************************************************
# Create benchmark porfolios
#*****************************************************************
# benchmarks <- runBenchmarkPortfolios()


#*****************************************************************
# Create Report
#******************************************************************    
models = rev(models)

strategy.performance.snapshoot(models, T)
# strategy.performance.snapshoot(benchmarks, T)
