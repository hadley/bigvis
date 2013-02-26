context("Binned vectors")

bv <- function(...) {
  BigVis$BinnedVectors$new(list(...))
}

test_that("square nbins correct", {
  g <- grouped(1:10, 1)
  expect_equal(bv(g)$nbins(), 11)
  expect_equal(bv(g, g)$nbins(), 11 ^ 2)
  expect_equal(bv(g, g, g)$nbins(), 11 ^ 3)
})

test_that("rectangular nbins correct", {
  g11 <- grouped(1:10, 1)
  g2 <- grouped(rep(1, 10), 1)

  expect_equal(bv(g2, g11)$nbins(), 22)
  expect_equal(bv(g11, g2)$nbins(), 22)
})

test_that("diagonal nbins correct", {
  x <- runif(1e3)
  y <- x + runif(1e3, -0.2, 0.2)
  z <- rnorm(1e3, x)

  gx <- grouped(x, 0.1)
  gy <- grouped(y, 0.1)

  expect_equal(gx$nbins(), 11)
  expect_equal(gy$nbins(), 15)

  bvs <- bv(gx, gy)
  expect_equal(bvs$nbins(), 165)

  bins <- vapply(seq_along(x) - 1, bvs$bin_i, integer(1))
  expect_true(all(bins <= 165))
})

test_that("bin and unbin are symmetric", {
  g <- grouped(1:10, 1)
  bvs <- bv(g, g)

  for(x in 1:10) {
    for(y in 1:10) {
      bin <- bvs$bin(c(x, y))
      expect_equal(bvs$unbin(bin), c(x, y), expected.label = c(x, y))
    }
  }
})
