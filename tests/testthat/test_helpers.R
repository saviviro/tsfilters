library(tsfilters)
context("Helper functions")

test_that("get_new_start works correctly", {
  expect_equal(get_new_start(y_start=c(1999, 1), y_freq=4, steps_forward=2), c(1999, 3))
  expect_equal(get_new_start(y_start=c(1999, 1), y_freq=4, steps_forward=5), c(2000, 2))
  expect_equal(get_new_start(y_start=c(1989, 1), y_freq=4, steps_forward=15), c(1992, 4))

  expect_equal(get_new_start(y_start=c(1999, 12), y_freq=12, steps_forward=1), c(2000, 1))
  expect_equal(get_new_start(y_start=c(1999, 12), y_freq=12, steps_forward=13), c(2001, 1))
  expect_equal(get_new_start(y_start=c(1999, 12), y_freq=12, steps_forward=14), c(2001, 2))

  expect_equal(get_new_start(y_start=c(1999, 50), y_freq=52, steps_forward=2), c(1999, 52))
  expect_equal(get_new_start(y_start=c(1999, 50), y_freq=52, steps_forward=4), c(2000, 2))
  expect_equal(get_new_start(y_start=c(2000, 51), y_freq=52, steps_forward=52), c(2001, 51))

  expect_equal(get_new_start(y_start=c(1999, 50), y_freq=250, steps_forward=200), c(1999, 250))
  expect_equal(get_new_start(y_start=c(1999, 200), y_freq=250, steps_forward=101), c(2000, 51))
  expect_equal(get_new_start(y_start=c(2000, 50), y_freq=250, steps_forward=500), c(2002, 50))
})
