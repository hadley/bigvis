context("Breaks")

last <- function(x) x[length(x)]

test_that("breaks includes max value, only if on border", {
  expect_equal(last(breaks(10, origin = 0, binwidth = 1)), 10)
  expect_equal(last(breaks(9.9, origin = 0, binwidth = 1)), 9)
})
