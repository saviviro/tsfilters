library(tsfilters)
context("Detrending filters")

data(INDPRO, package="tsfilters")
IP <- window(INDPRO, start=c(1951, 8)) # Start time at c(1954, 7), take h + p - 1 months back
IP_filtered <- hfilter(IP, h=24, p=12)
# p + h = 36, new start = c(1954, 7)

test_that("start date, end date, and frequency are correct in hfilter", {
  expect_equal(start(IP_filtered$cycle), c(1954, 7))
  expect_equal(start(IP_filtered$trend), c(1954, 7))
  expect_equal(end(IP_filtered$trend), c(2020, 12))
  expect_equal(frequency(IP_filtered$cycle), 12)
})

test_that("hfilter returns correct values", {
  expect_equal(IP_filtered$cycle[c(1, 12, 13, 100)], c(-1.2519900, -0.8680968, -1.6826269, 0.6357495), tol=1e-6)
  expect_equal(IP_filtered$trend[c(1, 22, 23, 200)], c(19.88919, 22.05204, 22.33727, 42.54467), tol=1e-6)
  expect_equal(IP_filtered$total[c(1, 32, 33, 300)], c(18.6372, 22.8466, 22.8189, 53.3157), tol=1e-4)
  expect_equal(IP_filtered$beta, c(3.4944675663, 1.8535140528, -0.4236875213, -0.2086187543, -0.3277114454, -0.1768639304,
                                   0.0003449284, -0.2360414100, 0.1847671459, -0.0354685097, -0.0953636749, -0.1938494956,
                                   0.6469979011), tol=1e-6)
})


IP_logdiff <-logdiff(INDPRO)

test_that("logdiff works correctly", {
  expect_equal(start(IP_logdiff), c(1919, 2))
  expect_equal(end(IP_logdiff), c(2020, 12))
  expect_equal(frequency(IP_logdiff), 12)
  expect_equal(IP_logdiff[c(1, 12, 13, 100, 700)], c(-0.045217430, 0.090066342, 0.000000000, 0.008066674, 0.008307682), tol=1e-6)
})
