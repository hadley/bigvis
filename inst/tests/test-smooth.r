context("Smooth")

tricube <- function(x) {
  x <- abs(x)
  ifelse(x > 1, 0, (1 - x ^ 3) ^ 3)
}
# plot(tricube, xlim = c(-1.5, 1.5))

test_that("factorised smooth equal to manual smooth", {
  grid <- as.matrix(expand.grid(x = 1:10, y = 1:10, KEEP.OUT.ATTRS = FALSE))
  z <- rep(0, nrow(grid))
  z[c(5, 23, 84)] <- 1

  z_x <- smooth_nd_1(grid, z, grid, 0, 3)
  z_y <- smooth_nd_1(grid, z, grid, 1, 3)

  z_xy <- smooth_nd_1(grid, z_x, grid, 1, 3)
  z_yx <- smooth_nd_1(grid, z_y, grid, 0, 3)
  z2 <- smooth_nd(grid, z, grid, c(3, 3))

  expect_equal(z_xy, z2)
  expect_equal(z_yx, z2)
})
