context("Binned vectors")

if (require("plyr")) {
  test_that("bins agree with plyr::id", {
    grid <- expand.grid(x = c(NA, seq(0, 0.5, by = 0.1)), y = c(NA, seq(0, 0.7, by = 0.1)))
    x <- grid$x
    y <- grid$y

    gx <- bin(x, 0.1)
    gy <- bin(y, 0.1)

    bv <- bins(gx, gy)
    bigvis <- sapply(seq_along(x) - 1, bv$bin_i)

    bin_x <- sapply(seq_along(x) - 1, gx$bin_i)
    bin_y <- sapply(seq_along(x) - 1, gy$bin_i)
    plyr <- as.vector(id(list(bin_x, bin_y)))

    expect_equal(bigvis + 1, plyr)
  })
}

test_that("square nbins correct", {
  g <- bin(1:10, 1)
  expect_equal(bins(g)$nbins(), 11)
  expect_equal(bins(g, g)$nbins(), 11 ^ 2)
  expect_equal(bins(g, g, g)$nbins(), 11 ^ 3)
})

test_that("rectangular nbins correct", {
  g11 <- bin(1:10, 1)
  g2 <- bin(rep(1, 10), 1)

  expect_equal(bins(g2, g11)$nbins(), 22)
  expect_equal(bins(g11, g2)$nbins(), 22)
})

test_that("diagonal nbins correct", {
  x <- runif(1e3)
  y <- x + runif(1e3, -0.2, 0.2)
  z <- rnorm(1e3, x)

  gx <- bin(x, 0.1)
  gy <- bin(y, 0.1)

  expect_equal(gx$nbins(), 11)
  expect_equal(gy$nbins(), 15)

  bvs <- bins(gx, gy)
  expect_equal(bvs$nbins(), 165)

  bins <- vapply(seq_along(x) - 1, bvs$bin_i, integer(1))
  expect_true(all(bins <= 165))
})

test_that("bin and unbin are symmetric", {
  g <- bin(-10:10, 1)
  bvs <- bins(g, g)

  grid <- expand.grid(x = -10:10, y = -10:10)
  bins <- unlist(Map(function(x, y) bvs$bin(c(x, y)), grid$x, grid$y))
  unbin <- t(vapply(bins, bvs$unbin, numeric(2)))
  colnames(unbin) <- c("x", "y")

  expect_equal(unbin, as.matrix(grid))
})

test_that("bin and unbin are symmetric with diff binning", {
  x <- c(-1, 5)
  y <- c(0.1, 1)

  bx <- bin(x, 1)
  by <- bin(y, 0.1)
  bvs <- bins(bx, by)

  grid <- expand.grid(
    x = breaks(bx)[-1] + 1 / 2,
    y = breaks(by)[-1] + 0.1 / 2)

  bins <- unlist(Map(function(x, y) bvs$bin(c(x, y)), grid$x, grid$y))
  unbin <- t(vapply(bins, bvs$unbin, numeric(2)))
  colnames(unbin) <- c("x", "y")

  expect_equal(unbin, as.matrix(grid))
})
