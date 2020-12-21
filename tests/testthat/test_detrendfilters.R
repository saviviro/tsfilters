library(tsfilters)
context("Detrending filters")

data(INDPRO, package="tsfilters")
IP <- window(INDPRO, start=c(1951, 8)) # Start time at c(1954, 7), take h + p - 1 months back
IP_filtered <- hfilter(IP, h=24, p=12)
# p + h = 36, new start = c(1954, 7)

test_that("start date, end date, and frequency are correct in hfilter", {
  expect_equal(start(IP_filtered$cycle), c(1954, 7))
  expect_equal(start(IP_filtered$trend), c(1954, 7))
  expect_equal(end(IP_filtered$trend), c(2020, 9))
  expect_equal(frequency(IP_filtered$cycle), 12)
})

test_that("hfilter returns correct values", {
  expect_equal(IP_filtered$cycle[c(1, 12, 13, 100)], c(-1.1959292, -0.8106173, -1.6517922, 0.7567731), tol=1e-6)
  expect_equal(IP_filtered$trend[c(1, 22, 23, 200)], c(19.83313, 21.98589, 22.27820, 42.54808), tol=1e-6)
  expect_equal(IP_filtered$total[c(1, 32, 33, 300)], c(18.6372, 22.8466, 22.8189, 53.3157), tol=1e-4)
  expect_equal(IP_filtered$beta, c(3.4192725, 1.8513850, -0.4247116, -0.1865374, -0.3071691, -0.1447882, -0.1191900,
                                   -0.2067206, 0.2112375, 0.0228795, -0.1657799, -0.2185750, 0.67747807), tol=1e-6)
})


IP_logdiff <-logdiff(INDPRO)

test_that("logdiff works correctly", {
  expect_equal(start(IP_logdiff), c(1919, 2))
  expect_equal(end(IP_logdiff), c(2020, 9))
  expect_equal(frequency(IP_logdiff), 12)
  expect_equal(IP_logdiff[c(1, 12, 13, 100, 700)], c(-0.045217430, 0.090066342, 0.000000000, 0.008066674, 0.008307682), tol=1e-6)
})
