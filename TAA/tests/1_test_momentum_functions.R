# UNIT TESTING OF FUNCTIONS IN MOMENTUM.R
# ===============================================

# Create test price data for Assets A and B
dates <- as.Date(c('2010-01-01','2010-01-02','2010-01-03','2010-01-04','2010-01-05',
                   '2010-01-06','2010-01-07','2010-01-08','2010-01-09','2010-01-10'))

pricea <- c(1,2,3,2,5,5,3,10,2,3)
priceb <- c(5,2,9,8,4,3,4,8,1,7)

test.prices <- xts(pricea,dates)
test.prices <- merge(test.prices, priceb)
names(test.prices) <- c('A.Close','B.Close')

#*************************************************
# Test calculateTRMomentum()
#*************************************************
test_that("calculateTRMomentum works for multiple asset price series", {
  a.test.result <- c(NA,NA,2,0,(2/3),1.5,-0.4,1,-(1/3),-0.7)
  b.test.result <- c(NA,NA,0.8,3,(4/9)-1,-0.625,0,(8/3)-1,-0.75,-0.125)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result)
  names(test.result) <- c('A.Close','B.Close')
  
  tr.actual <- calculateTRMomentum(test.prices, 2)
  
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})


#*************************************************
# Test calculateSMADifferential()
#*************************************************
test_that("calculateSMADifferential works for multiple asset price series", {
  a.test.result <- c(NA,NA,NA,0.25,0.16666667,0.33333333,0.06666667,
                     0.13043478,0.20000000,-0.44444444)
  b.test.result <- c(NA,NA,NA,0.41666667,0.04347826,-0.41666667,-0.26315789,
                     0.26315789,0.125,-0.2)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result)
  names(test.result) <- c('A.Close','B.Close')
  
  slowfastratio <<- 2
  tr.actual <- calculateSMADifferential(test.prices,4)
  
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})

#*************************************************
# Test calculatePriceToSMADifferential()
#*************************************************
test_that("calculatePriceToSMADifferential works for multiple asset price series", {
  a.test.result <- c(NA,NA,NA,0,0.6666667,0.3333333,-0.2,0.7391304,
                     -0.6,-0.3333333)
  b.test.result <- c(NA,NA,NA,0.3333333,-0.3043478,-0.5,-0.1578947,
                     0.6842105,-0.75,0.4)                  
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result)
  names(test.result) <- c('A.Close','B.Close')
  
  tr.actual <- calculatePriceToSMADifferential(test.prices,4)
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})

#*************************************************
# Test calculateInstantaneousSlope()
#*************************************************
test_that("calculateInstantaneousSlope works for multiple asset price series", {
  a.test.result <- c(NA,NA,NA,NA,0.5,0.25,0,0.5333333,-0.1304348,-0.1)
  b.test.result <- c(NA,NA,NA,NA,-0.04166667,0.04347826,-0.20833333,0,-0.15789474,0.25)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result)
  names(test.result) <- c('A.Close','B.Close')
  
  tr.actual <- calculateInstantaneousSlope(test.prices,4)
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})

#*************************************************
# Test calculatePercentileRank()
#*************************************************
test_that("calculatePercentileRank works for multiple asset price series", {
  a.test.result <- c(NA,NA,NA,0.5,0.875,0.75,0.375,0.875,0.125,0.5)
  b.test.result <- c(NA,NA,NA,0.625,0.375,0.125,0.5,0.875,0.125,0.625)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result)
  names(test.result) <- c('A.Close','B.Close')
  
  tr.actual <- calculatePercentileRank(test.prices,4)
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})

#*************************************************
# Test calculateZScore()
#*************************************************
test_that("calculateZScore works for multiple asset price series", {
  a.test.result <- c(NA,NA,NA,0,1.4142136,0.8333333,-0.5,1.4232712,
                     -0.8429272,-0.4057513)
  b.test.result <- c(NA,NA,NA,0.6324555,-0.5296549,-1.0190493,-0.3382407,
                     1.4657098,-1.0190493,0.6324555)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result)
  names(test.result) <- c('A.Close','B.Close')
  
  tr.actual <- calculateZScore(test.prices,4)
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})

#*************************************************
# Test calculateZDist()
#*************************************************
test_that("calculateZScore works for multiple asset price series", {
  a.test.result <- c(NA,NA,NA,0.5,0.9213504,0.7976716,0.3085375,0.9226712,
                     0.1996346,0.3424627)
  b.test.result <- c(NA,NA,NA,0.7364554,0.2981756,0.1540898,0.3675909,
                     0.9286363,0.1540898,0.7364554)
  test.result <- xts(a.test.result,dates)
  test.result <- merge(test.result, b.test.result)
  names(test.result) <- c('A.Close','B.Close')
  
  tr.actual <- calculateZDistribution(test.prices,4)
  expect_that(tr.actual, equals(test.result, tolerance = 0.00001))
})
