context("Grouping: 2d")

test_that("Two NAs gets bin 0", {
  expect_equal(group_rect(NA, NA, 1, 1, 0, 0), 0)
})

test_that("Sequential locations get sequential groups", {
  grid <- expand.grid(x = c(NA, 1:2), y = c(NA, 1:2))
  expect_equal(group_rect(grid$x, grid$y, 1, 1, 0.5, 0.5), 0:8)
})
