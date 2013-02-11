context("Summary: 1d")

test_that("summarise_1d counts small vectors accurately", {
  x <- c(NA, 0:10)
  s1 <- summarise_1d(x, summary = "count", binwidth = 1, origin = -0.5)
  # Pathological origin: need to add extra bin on end, because they're
  # right open, left closed
  s2 <- summarise_1d(x, summary = "count", binwidth = 1, origin = 0)

  expect_equal(s1$x, c(NA, 0:10 - 0.5))
  expect_equal(s2$x, c(NA, 0:10))

  expect_equal(s1$count, rep(1, length(x)))
  expect_equal(s2$count, rep(1, length(x)))
})

test_that("weights modify counts", {
  x <- c(NA, 0:10)
  w <- rep(2, length(x))
  s <- summarise_1d(x, w = w, summary = "count", binwidth = 1)

  expect_equal(s$x, c(NA, -0.5 + 0:10))
  expect_equal(s$count, rep(2, length(x)))
})

test_that("z affects sums, but not counts", {
  x <- c(NA, 0:10)
  z <- 0:11
  s <- summarise_1d(x, z, summary = "sum", binwidth = 1)

  expect_equal(s$count, rep(1, length(x)))
  expect_equal(s$sum, z)
})
