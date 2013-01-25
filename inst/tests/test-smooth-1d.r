context("Smooth: 1d")

density_d <- function(x, z, bw, from = min(x) - 3 * bw, to = max(x) + 3 * bw, n = 512) {
  suppressWarnings(density(x, bw = bw, weights = z, from = from, to = to,
    n = n)$y)
}

density_s <- function(x, z, bw, from = min(x) - 3 * bw, to = max(x) + 3 * bw, n = 512) {
  grid <- seq(from, to, length = n)
  smooth_1d_normal(x, z, grid, bw, standardise = FALSE)
}

test_that("for single point, agrees with dnorm", {
  s <- density_s(0, 1, 0.25, from = -1, to = 1, n = 256)
  grid <- seq(-1, 1, length = 256)

  expect_equal(s, dnorm(grid, sd = 0.25))

  # smooth_1d_normal is more numerically accurate than density
  # d <- density_d(0, 1, 0.25, from = -1, to = 1, n = 256)
  # expect_equal(d, dnorm(grid, sd = 0.25))
})

test_that("more multiple points, agrees with density", {
  x <- 1:10
  z <- rep(c(1, 2), length = length(x))

  d <- density_d(x, z, 0.1)
  s <- density_s(x, z, 0.1)

  expect_equal(s, d, tol = 0.01)
})

test_that("when standardised, small binwidth keeps values equal", {
  x <- seq(0, 1, length = 100)
  z <- runif(length(x))

  s <- smooth_1d_normal(x, z, x, 1/1000)
  expect_equal(s, z)
})
