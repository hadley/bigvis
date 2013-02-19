context("Grouping: 1d")

test_that("NAs belong to group 0", {
  x <- NA
  expect_equal(group_fixed(x, 1), 0)
})

test_that("Inf and -Inf belong to group 0", {
  x <- c(-Inf, Inf)
  expect_equal(group_fixed(x, 1), c(0, 0))
})

test_that("Out of range values belong to group 0", {
  expect_equal(group_fixed(-10, 1), 0)
})

test_that("Positive integers unchanged if origin is 1", {
  expect_equal(group_fixed(1:10, 1, 1), 1:10)
})
