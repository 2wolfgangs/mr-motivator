#*****************************************************************
# Momentum Analysis
# Uses code from https://github.com/systematicinvestor/SIT
#*****************************************************************
library('ProjectTemplate')
load.project()
source('src/momentum.R')

#*****************************************************************
# Code Strategies
#******************************************************************
prices <- data$prices  
n <- ncol(prices)

models <- list()

# find period ends
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]

# Momentum parameters
n.top <- 3      # no of momentum positions to hold     
# Momentum lookback (1,3,6,9,12 months)
momlookback <- c(1*22,3*22,6*22,9*22,12*22) 
# momlookback <- 6 * 22
# n.mom <- 6 * 22
slowfastratio <- 10


#*****************************************************************
# Total return momentum 
#*****************************************************************

totalreturn <- StandardisedMomentum(function(x) trMomentum(x))

data$weight[] <- NA
data$weight[period.ends,] <- ntop(totalreturn[period.ends,], n.top)   
models$totalreturn <- bt.run.share(data, clean.signal=F, trade.summary=T)
combinedweight <- data$weight

#*****************************************************************
# Total return less most recent month
#******************************************************************
trx1 <- StandardisedMomentum(function(x) trx1Momentum(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(trx1[period.ends,], n.top)   
models$trx1 <- bt.run.share(data, clean.signal=F, trade.summary=T)
# combinedweight <- combinedweight + data$weight

#*****************************************************************
# SMA differential
#*****************************************************************
smadiff <- StandardisedMomentum(function(x) SMADifferential(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(smadiff[period.ends,], n.top)  
models$smadiff <- bt.run.share(data, clean.signal=F, trade.summary=T)
combinedweight <- combinedweight + data$weight

#*****************************************************************
# Price to SMA differential
#*****************************************************************
pricesmadiff <- StandardisedMomentum(function(x) priceToSMADifferential(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(pricesmadiff[period.ends,], n.top)  
models$pricesmadiff <- bt.run.share(data, clean.signal=F, trade.summary=T)
combinedweight <- combinedweight + data$weight

#*****************************************************************
# Instantaneous Slope
# Daily rate of change of SMA 
#*****************************************************************
instslope <- StandardisedMomentum(function(x) instantaneousSlope(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(instslope[period.ends,], n.top)  
models$instslope <- bt.run.share(data, clean.signal=F, trade.summary=T)
combinedweight <- combinedweight + data$weight

#*****************************************************************
# Percentile Rank
# See: http://en.wikipedia.org/wiki/Percentile_rank
#*****************************************************************
percentrank <- StandardisedMomentum(function(x) percentileRank(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(percentrank[period.ends,], n.top)  
models$percentrank <- bt.run.share(data, clean.signal=F, trade.summary=T)
# combinedweight <- combinedweight + data$weight

#*****************************************************************
# Z score
# The magnitude that the current price deviates
# from the average price over the period
#*****************************************************************
zscore <- StandardisedMomentum(function(x) zScore(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(zscore[period.ends,], n.top)  
models$zscore <- bt.run.share(data, clean.signal=F, trade.summary=T)
# combinedweight <- combinedweight + data$weight

#*****************************************************************
# Z distribution 
# A transformation of the z score to a percentile value on the 
# cumulative normal distribution
#*****************************************************************
zdist <- StandardisedMomentum(function(x) zDistribution(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(zdist[period.ends,], n.top)  
models$zdist <- bt.run.share(data, clean.signal=F, trade.summary=T)
# combinedweight <- combinedweight + data$weight

# Combined Portfolio
data$weight <- combinedweight / (length(models) - 4)
models$combo <- bt.run.share(data, clean.signal=F, trade.summary=T)


#*****************************************************************
# Create Report
#******************************************************************    
models = rev(models)

strategy.performance.snapshoot(models, T)
