#*****************************************************************
# Risk adjusted Momentum Analysis
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
# SharpeRatio
#*****************************************************************

sharpe <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateSharpeRAM(x,y)
                                                , price.data, mom.lookbacks)

models$sharpe2 <- runEqualWeightBacktest(sharpe,2,TRUE)
models$sharpe3 <- runEqualWeightBacktest(sharpe,3,TRUE)
models$sharpe4 <- runEqualWeightBacktest(sharpe,4,TRUE)
models$sharpe5 <- runEqualWeightBacktest(sharpe,5,FALSE)

#*****************************************************************
# Omega Ratio
#******************************************************************
omega <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateOmegaRAM(x,y) 
                                         , price.data, mom.lookbacks) 

models$omega2 <- runEqualWeightBacktest(omega,2,FALSE)
models$omega3 <- runEqualWeightBacktest(omega,3,FALSE)
models$omega4 <- runEqualWeightBacktest(omega,4,FALSE)
models$omega5 <- runEqualWeightBacktest(omega,5,FALSE)

#*****************************************************************
# Sortino Ratio
#*****************************************************************
sortino <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateSortinoRAM(x,y)
                                            , price.data, mom.lookbacks) 

models$sortino2 <- runEqualWeightBacktest(sortino,2,TRUE)
models$sortino3 <- runEqualWeightBacktest(sortino,3,TRUE)
models$sortino4 <- runEqualWeightBacktest(sortino,4,TRUE)
models$sortino5 <- runEqualWeightBacktest(sortino,5,FALSE)

#*****************************************************************
# DVR Ratio
#*****************************************************************
DVR <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateDVRRAM(x,y)
                                                 , price.data, mom.lookbacks) 

models$DVR2 <- runEqualWeightBacktest(DVR,2,TRUE)
models$DVR3 <- runEqualWeightBacktest(DVR,3,TRUE)
models$DVR4 <- runEqualWeightBacktest(DVR,4,TRUE)
models$DVR5 <- runEqualWeightBacktest(DVR,5,FALSE)

#*****************************************************************
# VaR
#*****************************************************************
VaR <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateVaRRAM(x,y)
                                              , price.data, mom.lookbacks) 

models$VaR2 <- runEqualWeightBacktest(VaR,2,TRUE)
models$VaR3 <- runEqualWeightBacktest(VaR,3,TRUE)
models$VaR4 <- runEqualWeightBacktest(VaR,4,TRUE)
models$VaR5 <- runEqualWeightBacktest(VaR,5,FALSE)

#*****************************************************************
# Conditional VaR
#*****************************************************************
CVaR <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateCVaRRAM(x,y)
                                                , price.data, mom.lookbacks) 

models$CVaR2 <- runEqualWeightBacktest(CVaR,2,TRUE)
models$CVaR3 <- runEqualWeightBacktest(CVaR,3,TRUE)
models$CVaR4 <- runEqualWeightBacktest(CVaR,4,TRUE)
models$CVaR5 <- runEqualWeightBacktest(CVaR,5,FALSE)

#*****************************************************************
# Return to max loss
#*****************************************************************
maxloss <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateMaxLossRAM(x,y)
                                           , price.data, mom.lookbacks) 

models$maxloss2 <- runEqualWeightBacktest(maxloss,2,TRUE)
models$maxloss3 <- runEqualWeightBacktest(maxloss,3,TRUE)
models$maxloss4 <- runEqualWeightBacktest(maxloss,4,TRUE)
models$maxloss5 <- runEqualWeightBacktest(maxloss,5,FALSE)

#*****************************************************************
# Ulcer index
#*****************************************************************
zdist <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateUlcerRAM(x,y)
                                          , price.data, mom.lookbacks) 

models$ulcer2 <- runEqualWeightBacktest(ulcer,2,TRUE)
models$ulcer3 <- runEqualWeightBacktest(ulcer,3,TRUE)
models$ulcer4 <- runEqualWeightBacktest(ulcer,4,TRUE)
models$ulcer5 <- runEqualWeightBacktest(ulcer,5,FALSE)

#*****************************************************************
# Gain to Pain
#*****************************************************************
gaintopain <- standardiseMomentumOverLookbacks(std.type, function(x,y) calculateGainToPainRAM(x,y)
                                          , price.data, mom.lookbacks) 

models$gaintopain2 <- runEqualWeightBacktest(gaintopain,2,TRUE)
models$gaintopain3 <- runEqualWeightBacktest(gaintopain,3,TRUE)
models$gaintopain4 <- runEqualWeightBacktest(gaintopain,4,TRUE)
models$gaintopain5 <- runEqualWeightBacktest(gaintopain,5,FALSE)

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
