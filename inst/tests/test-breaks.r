context("Breaks")

last <- function(x) x[length(x)]

test_that("breaks includes max value, only if on border", {
  expect_equal(last(breaks(10, origin = 0, binwidth = 1)), 10)
  expect_equal(last(breaks(9.99, origin = 0, binwidth = 1)), 9)
})

test_that("breaks includes max value even when origin != 0", {
  expect_equal(last(breaks(10.5, origin = 0.5, binwidth = 1)), 10.5)
  expect_equal(last(breaks(10.49, origin = 0.5, binwidth = 1)), 9.5)
})
