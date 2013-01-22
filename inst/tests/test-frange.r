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
