context("Grouping: 1d")

test_that("NAs belong to group 0", {
  x <- NA
  expect_equal(group_fixed(x, 1), 0)
  expect_equal(group_integer(x, 0), 0)
  expect_equal(group_breaks(x, c(0, 1, 2)), 0)
})

test_that("Out of range values belong to group 0", {
  expect_equal(group_fixed(-10, 1), 0)
  expect_equal(group_integer(-10, 0), 0)
  expect_equal(group_breaks(-10, c(0, 1, 2)), 0)
})
