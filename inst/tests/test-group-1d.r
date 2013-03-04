context("Grouping: 1d")

group <- function(x, width, origin = NULL) {
  g <- bin(x, width, origin)
  vapply(seq_along(x) - 1, g$bin_i, integer(1))
}

test_that("NAs belong to group 0", {
  x <- NA_real_
  expect_equal(group(x, 1, 0), 0L)
})

test_that("Inf and -Inf belong to group 0", {
  x <- c(-Inf, Inf)
  expect_equal(group(x, 1, 0), c(0, 0))
})

test_that("Out of range values belong to group 0", {
  expect_equal(group(-10, 1, 0), 0)
})

test_that("Positive integers unchanged if origin is 1", {
  expect_equal(group(1:10, 1, 1), 1:10)
})
