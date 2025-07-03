library(tsfilters)
context("Detrending filters")

data(INDPRO, package="tsfilters")
IP1 <- window(INDPRO, start=c(1951, 8))
IP2 <- window(INDPRO, start=c(1980, 1), end=c(2010, 2))
IP3 <- window(INDPRO, start=c(1970, 12), end=c(2008, 1))

# Hamilton filter
H_filtered1 <- hfilter(IP1, h=24, p=12) # Original time at c(1951, 8), takes h + p - 1 months forward;
# p + h - 1 = 35 -> new start = c(1954, 7)
H_filtered2 <- hfilter(IP2, h=12, p=12) # Start time at c(1980, 1), takes h + p - 1 months forward;
# p + h - 1 = 23 -> new start = c(1981, 12)
H_filtered3 <- hfilter(IP3, h=11, p=2) # Start time at c(1970, 12), takes h + p - 1 months forward;
# p + h - 1 = 12 -> new start = c(1971, 12)

test_that("start date, end date, and frequency are correct in hfilter", {
  expect_equal(start(H_filtered1$cycle), c(1954, 7))
  expect_equal(start(H_filtered1$trend), c(1954, 7))
  expect_equal(end(H_filtered1$trend), c(2020, 12))
  expect_equal(frequency(H_filtered1$cycle), 12)

  expect_equal(start(H_filtered2$cycle), c(1981, 12))
  expect_equal(start(H_filtered2$trend), c(1981, 12))
  expect_equal(end(H_filtered2$trend), c(2010, 2))
  expect_equal(frequency(H_filtered2$cycle), 12)

  expect_equal(start(H_filtered3$cycle), c(1971, 12))
  expect_equal(start(H_filtered3$trend), c(1971, 12))
  expect_equal(end(H_filtered3$trend), c(2008, 1))
  expect_equal(frequency(H_filtered3$cycle), 12)
})


test_that("hfilter returns correct values", {
  expect_equal(H_filtered1$cycle[c(1, 12, 13, 100)], c(-1.2519900, -0.8680968, -1.6826269, 0.6357495), tol=1e-6)
  expect_equal(H_filtered1$trend[c(1, 22, 23, 200)], c(19.88919, 22.05204, 22.33727, 42.54467), tol=1e-6)
  expect_equal(H_filtered1$total[c(1, 32, 33, 300)], c(18.6372, 22.8466, 22.8189, 53.3157), tol=1e-4)
  expect_equal(H_filtered1$beta, c(3.4944675663, 1.8535140528, -0.4236875213, -0.2086187543, -0.3277114454, -0.1768639304,
                                   0.0003449284, -0.2360414100, 0.1847671459, -0.0354685097, -0.0953636749, -0.1938494956,
                                   0.6469979011), tol=1e-6)
})

test_that("hpfilter works correctly", {
  HP_filtered1 <- hpfilter(IP1, lambda=1600, type="two-sided")
  expect_equal(start(HP_filtered1$cycle), c(1951, 8), tol=1e-6)
  expect_equal(end(HP_filtered1$cycle), c(2020, 12), tol=1e-6)
  expect_equal(frequency(HP_filtered1$cycle), 12)
  expect_equal(HP_filtered1$cycle[c(1, 101, 200)], c(0.09324078, 0.72375908, -0.10277923), tol=1e-6)
  expect_equal(HP_filtered1$trend[c(1, 101, 200)], c(17.21476, 23.53514, 37.70018), tol=1e-6)

  IP4 <- window(INDPRO, start=c(2000, 1), end=c(2008, 12))
  HP_filtered1_oneside <- hpfilter(IP4, lambda=1600, type="one-sided")
  expect_equal(start(HP_filtered1_oneside$cycle), c(2000, 3), tol=1e-6)
  expect_equal(end(HP_filtered1_oneside$cycle), c(2008, 12), tol=1e-6)
  expect_equal(frequency(HP_filtered1_oneside$cycle), 12)
  expect_equal(HP_filtered1_oneside$cycle[c(1, 2, 100)], c(0.01039892, 0.11459766, -1.66354255), tol=1e-6)
  expect_equal(HP_filtered1_oneside$trend[c(1, 2, 100)], c(94.7876, 95.3662, 104.5080), tol=1e-4)
})

test_that("logdiff works correctly", {
  IP_logdiff <-logdiff(INDPRO)
  expect_equal(start(IP_logdiff), c(1919, 2))
  expect_equal(end(IP_logdiff), c(2020, 12))
  expect_equal(frequency(IP_logdiff), 12)
  expect_equal(IP_logdiff[c(1, 12, 13, 100, 700)], c(-0.045217430, 0.090066342, 0.000000000, 0.008066674, 0.008307682), tol=1e-6)
})
