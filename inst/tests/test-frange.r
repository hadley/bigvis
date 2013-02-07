context("frange")

test_that("frange agrees with range", {
  x <- rnorm(1e4)
  expect_equal(frange(x), range(x))
})

test_that("frange uses cache if present", {
  x <- rnorm(1e4)
  attr(x, "range") <- c(0, 10)
  expect_equal(frange(x), c(0, 10))
})

test_that("frange ignores NA and infinities by default", {
  x <- c(1, NA, Inf, -Inf)
  expect_equal(frange(x), c(1, 1))
})
