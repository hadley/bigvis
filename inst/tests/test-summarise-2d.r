context("Summary: 2d")

test_that("summarise2d counts small vectors accurately", {
  grid <- expand.grid(x = c(NA, 1:2), y = c(NA, 1:2))
  summarise2d(grid$x, grid$y, x_binwidth = 1, y_binwidth = 1)

  group_rect(grid$x, grid$y, x_width = 1, y_width = 1, 0.5, 0.5)

  x <- c(NA, 0:5)
  y <- rep(1L, length(x))
  sxy <- summarise2d(x, y, summary = "count", x_binwidth = 1, y_binwidth = 1)
  syx <- summarise2d(y, x, summary = "count", x_binwidth = 1, y_binwidth = 1)

})
