context("Grouping: 1d")

test_that("NAs belong to group 0", {
  x <- c(NA, 0, 1)
  expect_equal(group_fixed(x, 1), c(0, 1, 2))
  expect_equal(group_integer(x, 0), c(0, 1, 2))
  expect_equal(group_breaks(x, c(0, 1, 2)), c(0, 1, 2))
})

