#*****************************************************************
# Raw Momentum Analysis
# Uses code from https://github.com/systematicinvestor/SIT
#*****************************************************************
library('ProjectTemplate')
load.project()

#*****************************************************************
# Code Strategies
#******************************************************************
prices <- data$prices  
n <- ncol(prices)

models <- list()

# find period ends
period.ends <- endpoints(prices, 'months')
period.ends <- period.ends[period.ends > 0]
 
# Momentum lookback (1,3,6,9,12 months)
momlookback <- c(1*22,3*22,6*22,9*22,12*22) 
slowfastratio <- 10


#*****************************************************************
# Total return momentum 
#*****************************************************************

totalreturn <- StandardisedMomentum(function(x) TRMomentum(x))

models$totalreturn2 <- RunEqualWeightBacktest(totalreturn,2,TRUE)
models$totalreturn3 <- RunEqualWeightBacktest(totalreturn,3,TRUE)
models$totalreturn4 <- RunEqualWeightBacktest(totalreturn,4,TRUE)
models$totalreturn5 <- RunEqualWeightBacktest(totalreturn,5,FALSE)

#*****************************************************************
# Total return less most recent month
#******************************************************************
trx1 <- StandardisedMomentum(function(x) TRx1Momentum(x)) 

models$trx2 <- RunEqualWeightBacktest(trx1,2,FALSE)
models$trx3 <- RunEqualWeightBacktest(trx1,3,FALSE)
models$trx4 <- RunEqualWeightBacktest(trx1,4,FALSE)
models$trx5 <- RunEqualWeightBacktest(trx1,5,FALSE)

#*****************************************************************
# SMA differential
#*****************************************************************
smadiff <- StandardisedMomentum(function(x) SMADifferential(x)) 

models$smadiff2 <- RunEqualWeightBacktest(smadiff,2,TRUE)
models$smadiff3 <- RunEqualWeightBacktest(smadiff,3,TRUE)
models$smadiff4 <- RunEqualWeightBacktest(smadiff,4,TRUE)
models$smadiff5 <- RunEqualWeightBacktest(smadiff,5,FALSE)

#*****************************************************************
# Price to SMA differential
#*****************************************************************
pricesmadiff <- StandardisedMomentum(function(x) PriceToSMADifferential(x)) 

models$pricesmadiff2 <- RunEqualWeightBacktest(pricesmadiff,2,TRUE)
models$pricesmadiff3 <- RunEqualWeightBacktest(pricesmadiff,3,TRUE)
models$pricesmadiff4 <- RunEqualWeightBacktest(pricesmadiff,4,TRUE)
models$pricesmadiff5 <- RunEqualWeightBacktest(pricesmadiff,5,FALSE)

#*****************************************************************
# Instantaneous Slope
# Daily rate of change of SMA 
#*****************************************************************
instslope <- StandardisedMomentum(function(x) InstantaneousSlope(x)) 

models$instslope2 <- RunEqualWeightBacktest(instslope,2,TRUE)
models$instslope3 <- RunEqualWeightBacktest(instslope,3,TRUE)
models$instslope4 <- RunEqualWeightBacktest(instslope,4,TRUE)
models$instslope5 <- RunEqualWeightBacktest(instslope,5,FALSE)

#*****************************************************************
# Percentile Rank
# See: http://en.wikipedia.org/wiki/Percentile_rank
#*****************************************************************
percentrank <- StandardisedMomentum(function(x) PercentileRank(x)) 

models$percentrank2 <- RunEqualWeightBacktest(percentrank,2,FALSE)
models$percentrank3 <- RunEqualWeightBacktest(percentrank,3,FALSE)
models$percentrank4 <- RunEqualWeightBacktest(percentrank,4,FALSE)
models$percentrank5 <- RunEqualWeightBacktest(percentrank,5,FALSE)

#*****************************************************************
# Z score
# The magnitude that the current price deviates
# from the average price over the period
#*****************************************************************
zscore <- StandardisedMomentum(function(x) ZScore(x)) 

models$zscore2 <- RunEqualWeightBacktest(zscore,2,FALSE)
models$zscore3 <- RunEqualWeightBacktest(zscore,3,FALSE)
models$zscore4 <- RunEqualWeightBacktest(zscore,4,FALSE)
models$zscore5 <- RunEqualWeightBacktest(zscore,5,FALSE)

#*****************************************************************
# Z distribution 
# A transformation of the z score to a percentile value on the 
# cumulative normal distribution
#*****************************************************************
zdist <- StandardisedMomentum(function(x) ZDistribution(x)) 

models$zdist2 <- RunEqualWeightBacktest(zdist,2,FALSE)
models$zdist3 <- RunEqualWeightBacktest(zdist,3,FALSE)
models$zdist4 <- RunEqualWeightBacktest(zdist,4,FALSE)
models$zdist5 <- RunEqualWeightBacktest(zdist,5,FALSE)

#*****************************************************************
# Equal weighted portfolio backtest
# Equal weighted 
#*****************************************************************
models$combo <- RunComboBacktest()

#*****************************************************************
# Create Report
#******************************************************************    
models = rev(models)

strategy.performance.snapshoot(models, T)
