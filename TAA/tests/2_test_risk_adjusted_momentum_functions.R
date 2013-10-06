# UNIT TESTING OF FUNCTIONS IN RISK_ADJUSTED_MOMENTUM.R
# =====================================================

# Create test price data for Assets A and B
dates <- as.Date(c('2010-01-01','2010-01-02','2010-01-03','2010-01-04','2010-01-05',
                   '2010-01-06','2010-01-07','2010-01-08','2010-01-09','2010-01-10'))

pricea <- c(1,2,3,2,5,5,3,10,2,3)
priceb <- c(5,2,9,8,4,3,4,8,1,7)

test.prices <- xts(pricea,dates)
test.prices <- merge(test.prices, priceb)
names(test.prices) <- c('A.Close','B.Close')

#*************************************************
# Test calculateSharpeRAM()
#*************************************************
test_that("calculateSharpeRAM works for multiple asset price series", {
  a.test.result <- c(NA,NA,NA,NA,NA,NA,NA,NA,0.442925,0.392177)
  b.test.result <- c(NA,NA,NA,NA,NA,NA,NA,NA,0.220473,0.475072)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result)
  names(test.result) <- c('A.Close','B.Close')
  
  tr.actual <- calculateSharpeRAM(test.prices, 8)
  
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})

test_that("calculateOmegaRAM works for multiple asset price series", {
  a.test.result <- c(NA,NA,NA,NA,NA,NA,NA,NA,3.478261,3.152174)
  b.test.result <- c(NA,NA,NA,NA,NA,NA,NA,NA,2.068966,6.24)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result)
  names(test.result) <- c('A.Close','B.Close')
  
  tr.actual <- calculateOmegaRAM(test.prices, 8)
  
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})

test_that("calculateSortinoRAM works for multiple asset price series", {
  a.test.result <- c(NA,NA,NA,NA,NA,NA,NA,NA,1.407515,1.222315)
  b.test.result <- c(NA,NA,NA,NA,NA,NA,NA,NA,0.7330907,3.080043)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result)
  names(test.result) <- c('A.Close','B.Close')
  
  tr.actual <- calculateSortinoRAM(test.prices, 8)
  
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})

test_that("calculateDVRRAM works for multiple asset price series", {
  a.test.result <- c(NA,NA,NA,NA,NA,NA,NA,NA,1.202224,0.364164)
  b.test.result <- c(NA,NA,NA,NA,NA,NA,NA,NA,-0.07874036,-0.24432286)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result)
  names(test.result) <- c('A.Close','B.Close')
  
  tr.actual <- calculateDVRRAM(test.prices, 8)
  
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})