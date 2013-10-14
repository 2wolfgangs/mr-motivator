# UNIT TESTING OF FUNCTIONS IN STANDARDISATION.R
# ===============================================

# Create test price data for 4 assets 
dates <- as.Date(c('2010-01-01','2010-01-02'))
moma <- c(1,-1)
momb <- c(-1,5)
momc <- c(2,-5)
momd <- c(5,-8)

test.mom <- xts(moma,dates)
test.mom <- merge(test.mom, momb, momc, momd)
names(test.mom) <- c('A','B','C','D')

#*************************************************
# Test standardiseAbsoluteValue()
#*************************************************
test_that("standardiseAbsoluteValue works for multiple asset price series", {
  a.test.result <- c(0.1111111,-0.0526316)
  b.test.result <- c(-0.1111111,0.2631579)
  c.test.result <- c(0.2222222,-0.2631579)
  d.test.result <- c(0.5555556,-0.4210526)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result, c.test.result,
                       d.test.result)
  names(test.result) <- c('A','B','C','D')
  
  tr.actual <- standardiseAbsoluteValue(test.mom)
  
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})

#*************************************************
# Test standardiseZScore()
#*************************************************
test_that("standardiseZScore works for multiple asset price series", {
  a.test.result <- c(-0.3,0.2224237)
  b.test.result <- c(-1.1,1.290057)
  c.test.result <- c(0.1,-0.489332)
  d.test.result <- c(1.3,-1.023149)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result, c.test.result,
                       d.test.result)
  names(test.result) <- c('A','B','C','D')
  
  tr.actual <- standardiseZScore(test.mom)
  
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})

#*************************************************
# Test standardiseZDistribution()
#*************************************************
test_that("standardiseZDistribution works for multiple asset price series", {
  a.test.result <- c(0.3820886,0.588008)
  b.test.result <- c(0.1356661,0.9014846)
  c.test.result <- c(0.5398278,0.3123033)
  d.test.result <- c(0.9031995,0.1531187)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result, c.test.result,
                       d.test.result)
  names(test.result) <- c('A','B','C','D')
  
  tr.actual <- standardiseZDistribution(test.mom)
  
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})

#*************************************************
# Test standardiseMomentumAcrossAssets()
#*************************************************
test_that("standardiseMomentumAcrossAssets works for multiple asset price series", {
  a.test.result <- c(0.1948654,0.3007845)
  b.test.result <- c(0.06919,0.461138)
  c.test.result <- c(0.2753125,0.1597529)
  d.test.result <- c(0.4606323,0.078325)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result, c.test.result,
                       d.test.result)
  names(test.result) <- c('A','B','C','D')
  
  tr.actual <- standardiseMomentumAcrossAssets(test.mom)
  
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})
