library(tsfilters)
context("Detrending filters")

IP <- window(INDPRO, start=c(1951, 8)) # Start time at c(1954, 7), take h + p - 1 months back
IP_filtered <- hfilter(IP, h=24, p=12)
# p + h = 36, new start = c(1954, 7)

test_that("start date and frequency are correct in hfilter", {
  expect_equal(start(IP_filtered$cycle), c(1954, 7))
  expect_equal(start(IP_filtered$trend), c(1954, 7))
  expect_equal(frequency(IP_filtered$cycle), 12)
})
