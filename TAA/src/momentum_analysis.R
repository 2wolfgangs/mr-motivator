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

# Get prices from data environment
prices <- data$prices  

# Find period ends using SIT function
period.ends <- endpoints(prices, 'months')
period.ends <- period.ends[period.ends > 0]

#*****************************************************************
# Total return momentum 
#*****************************************************************

totalreturn <- standardiseMomentumOverLookbacks(function(x) calculateTRMomentum(x), mom.lookbacks)

models$totalreturn2 <- runEqualWeightBacktest(totalreturn,2,TRUE)
models$totalreturn3 <- runEqualWeightBacktest(totalreturn,3,TRUE)
models$totalreturn4 <- runEqualWeightBacktest(totalreturn,4,TRUE)
models$totalreturn5 <- runEqualWeightBacktest(totalreturn,5,FALSE)

#*****************************************************************
# Total return less most recent month
#******************************************************************
trx1 <- standardiseMomentumOverLookbacks(function(x) calculateTRx1Momentum(x), mom.lookbacks) 

models$trx2 <- runEqualWeightBacktest(trx1,2,FALSE)
models$trx3 <- runEqualWeightBacktest(trx1,3,FALSE)
models$trx4 <- runEqualWeightBacktest(trx1,4,FALSE)
models$trx5 <- runEqualWeightBacktest(trx1,5,FALSE)

#*****************************************************************
# SMA differential
#*****************************************************************
smadiff <- standardiseMomentumOverLookbacks(function(x) calculateSMADifferential(x), mom.lookbacks) 

models$smadiff2 <- runEqualWeightBacktest(smadiff,2,TRUE)
models$smadiff3 <- runEqualWeightBacktest(smadiff,3,TRUE)
models$smadiff4 <- runEqualWeightBacktest(smadiff,4,TRUE)
models$smadiff5 <- runEqualWeightBacktest(smadiff,5,FALSE)

#*****************************************************************
# Price to SMA differential
#*****************************************************************
pricesmadiff <- standardiseMomentumOverLookbacks(function(x) calculatePriceToSMADifferential(x), mom.lookbacks) 

models$pricesmadiff2 <- runEqualWeightBacktest(pricesmadiff,2,TRUE)
models$pricesmadiff3 <- runEqualWeightBacktest(pricesmadiff,3,TRUE)
models$pricesmadiff4 <- runEqualWeightBacktest(pricesmadiff,4,TRUE)
models$pricesmadiff5 <- runEqualWeightBacktest(pricesmadiff,5,FALSE)

#*****************************************************************
# Instantaneous Slope
# Daily rate of change of SMA 
#*****************************************************************
instslope <- standardiseMomentumOverLookbacks(function(x) calculateInstantaneousSlope(x), mom.lookbacks) 

models$instslope2 <- runEqualWeightBacktest(instslope,2,TRUE)
models$instslope3 <- runEqualWeightBacktest(instslope,3,TRUE)
models$instslope4 <- runEqualWeightBacktest(instslope,4,TRUE)
models$instslope5 <- runEqualWeightBacktest(instslope,5,FALSE)

#*****************************************************************
# Percentile Rank
# See: http://en.wikipedia.org/wiki/Percentile_rank
#*****************************************************************
percentrank <- standardiseMomentumOverLookbacks(function(x) calculatePercentileRank(x), mom.lookbacks) 

models$percentrank2 <- runEqualWeightBacktest(percentrank,2,FALSE)
models$percentrank3 <- runEqualWeightBacktest(percentrank,3,FALSE)
models$percentrank4 <- runEqualWeightBacktest(percentrank,4,FALSE)
models$percentrank5 <- runEqualWeightBacktest(percentrank,5,FALSE)

#*****************************************************************
# Z score
# The magnitude that the current price deviates
# from the average price over the period
#*****************************************************************
zscore <- standardiseMomentumOverLookbacks(function(x) calculateZScore(x), mom.lookbacks) 

models$zscore2 <- runEqualWeightBacktest(zscore,2,FALSE)
models$zscore3 <- runEqualWeightBacktest(zscore,3,FALSE)
models$zscore4 <- runEqualWeightBacktest(zscore,4,FALSE)
models$zscore5 <- runEqualWeightBacktest(zscore,5,FALSE)

#*****************************************************************
# Z distribution 
# A transformation of the z score to a percentile value on the 
# cumulative normal distribution
#*****************************************************************
zdist <- standardiseMomentumOverLookbacks(function(x) calculateZDistribution(x), mom.lookbacks) 

models$zdist2 <- runEqualWeightBacktest(zdist,2,FALSE)
models$zdist3 <- runEqualWeightBacktest(zdist,3,FALSE)
models$zdist4 <- runEqualWeightBacktest(zdist,4,FALSE)
models$zdist5 <- runEqualWeightBacktest(zdist,5,FALSE)

#*****************************************************************
# Equal weighted portfolio backtest
# Potfolio of all models, equal weighted.
#*****************************************************************
models$combo <- runComboBacktest()

#*****************************************************************
# Create Report
#******************************************************************    
models = rev(models)

strategy.performance.snapshoot(models, T)
