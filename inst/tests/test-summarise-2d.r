context("Summary: 2d")

test_that("grid counted accurately", {
  grid <- expand.grid(x = c(NA, 1:2), y = c(NA, 1:2))
  s <- summarise2d(grid$x, grid$y, x_binwidth = 1, y_binwidth = 1)

  expect_equal(s$count, rep(1, nrow(grid)))
})

test_that("diagonal counted correctly", {
  df <- data.frame(x = c(NA, 1:2), y = c(NA, 1:2))
  s <- summarise2d(df$x, df$y, x_binwidth = 1, y_binwidth = 1)
  s <- s[s$count == 1, ]

  expect_equal(nrow(s), nrow(df))
  expect_equal(s$x, s$y)
})

