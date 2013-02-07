context("Grouping: 1d")

test_that("NAs belong to group 0", {
  x <- NA
  expect_equal(group_fixed(x, 1), 0)
  expect_equal(group_integer(x, 0), 0)
  expect_equal(group_breaks(x, c(0, 1, 2)), 0)
})

test_that("Inf and -Inf belong to group 0", {
  x <- c(-Inf, Inf)
  expect_equal(group_fixed(x, 1), c(0, 0))
})

test_that("Out of range values belong to group 0", {
  expect_equal(group_fixed(-10, 1), 0)
  expect_equal(group_integer(-10, 0), 0)
  expect_equal(group_breaks(-10, c(0, 1, 2)), 0)
})

test_that("Positive integers unchanged if origin is 1", {
  expect_equal(group_fixed(1:10, 1, 1), 1:10)
  expect_equal(group_integer(1:10, 1), 1:10)

  # Have to add additional bin here - these are pathological breaks for
  # binning histograms, but if the right-most break is right closed, the
  # bin height will be falsely doubled.
  expect_equal(group_breaks(1:10, 1:11), 1:10)

})

test_that("GroupFixed: values inside breaks same as findInterval", {
  x <- runif(1e2)
  breaks <- seq(0, 1, length = 20)

  expect_equal(
    group_breaks(x, breaks),
    findInterval(x, breaks))

  # Behaviour differs to findIntervals for last break:
  # all group_breaks intervals are of the form [x, y)
  expect_equal(
    group_breaks(breaks, breaks),
    c(1:19, 0))
})

test_that("GroupFixed: values outside of breaks belong to group 0", {
  left <- c(NA, -1, -1e-3, -1e-6)
  right <- c(1, 10, 100, 1000)
  breaks <- seq(0, 1, length = 20)

  expect_equal(group_breaks(left, breaks), rep(0, 4))
  expect_equal(group_breaks(right, breaks), rep(0, 4))
})

