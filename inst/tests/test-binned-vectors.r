context("Binned vectors")

bv <- function(...) {
  BigVis$BinnedVectors$new(list(...))
}

if (require("plyr")) {
  test_that("bins agree with plyr::id", {
    grid <- expand.grid(x = c(NA, seq(0, 0.5, by = 0.1)), y = c(NA, seq(0, 0.7, by = 0.1)))
    x <- grid$x
    y <- grid$y

    gx <- bin(x, 0.1)
    gy <- bin(y, 0.1)

    bin_x <- sapply(seq_along(x) - 1, gx$bin_i)
    bin_y <- sapply(seq_along(x) - 1, gy$bin_i)

    bv <- BigVis$BinnedVectors$new(list(gx, gy))
    bigvis <- sapply(seq_along(x) - 1, bv$bin_i)
    plyr <- as.vector(id(list(bin_x, bin_y)))

    expect_equal(bigvis + 1, plyr)
  })
}

test_that("square nbins correct", {
  g <- bin(1:10, 1)
  expect_equal(bv(g)$nbins(), 11)
  expect_equal(bv(g, g)$nbins(), 11 ^ 2)
  expect_equal(bv(g, g, g)$nbins(), 11 ^ 3)
})

test_that("rectangular nbins correct", {
  g11 <- bin(1:10, 1)
  g2 <- bin(rep(1, 10), 1)

  expect_equal(bv(g2, g11)$nbins(), 22)
  expect_equal(bv(g11, g2)$nbins(), 22)
})

test_that("diagonal nbins correct", {
  x <- runif(1e3)
  y <- x + runif(1e3, -0.2, 0.2)
  z <- rnorm(1e3, x)

  gx <- bin(x, 0.1)
  gy <- bin(y, 0.1)

  expect_equal(gx$nbins(), 11)
  expect_equal(gy$nbins(), 15)

  bvs <- bv(gx, gy)
  expect_equal(bvs$nbins(), 165)

  bins <- vapply(seq_along(x) - 1, bvs$bin_i, integer(1))
  expect_true(all(bins <= 165))
})

test_that("bin and unbin are symmetric", {
  g <- bin(1:10, 1)
  bvs <- bv(g, g)

  grid <- expand.grid(x = 1:10, y = 1:10)
  bins <- unlist(Map(function(x, y) bvs$bin(c(x, y)), grid$x, grid$y))
  unbin <- t(vapply(bins, bvs$unbin, numeric(2)))
  colnames(unbin) <- c("x", "y")

  expect_equal(unbin, as.matrix(grid))
})
