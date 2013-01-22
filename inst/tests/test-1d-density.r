# context("1d smoothing")

# data <- data.frame(x = rnorm(100,0,100))
# compare_density <- function(x, binwidth = .01, bandwidth = .1) {
#   df <- data.frame(x)
#   binx <- bin_1d(df, "x", binwidth)
#   dens1 <- density_1d(binx, bandwidth)

#   dens2 <- density(x, bw = bandwidth)

#   d1 <- approxfun(dens1$x, dens1$y)
#   d2 <- approxfun(dens2$x, dens2$y)

#   grid <- seq(min(dens2$x), max(dens2$x), length = 500)
#   mse <- mean((d1(grid) - d2(grid)) ^ 2)

#   list(x = grid, y1 = d1(grid), y2 = d2(grid), mse = mse)
# }

# test_that("density1d similar to base::density", {
#   norm <- compare_density(rnorm(1e3))
#   unif <- compare_density(runif(1e3))

#   expect_true(norm$mse < 1e-3)
#   expect_true(unif$mse < 1e-3)
# })

# # Graphical exploration of errors
# if (FALSE) {
#   norm <- compare_density(rnorm(1e4))
#   plot(norm$x, norm$y1, type = "l")
#   lines(norm$x, norm$y2, col = "blue")

#   plot(norm$x, norm$y1 - norm$y2, type = "l")
#   abline(h = 0, col = "grey50")
#   rug(x)
# }
