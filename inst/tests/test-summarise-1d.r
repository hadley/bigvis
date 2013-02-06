context("Summary: 1d")

test_that("summarise1d counts small vectors accurately", {
  x <- c(NA, 0:10)
  s <- summarise1d(x, summary = "count", binwidth = 1)

  expect_equal(s$x, c(NA, 0.5 + 0:10))
  expect_equal(s$count, rep(1, length(x)))
})

test_that("weights modify counts", {
  x <- c(NA, 0:10)
  w <- rep(2, length(x))
  s <- summarise1d(x, w = w, summary = "count", binwidth = 1)

  expect_equal(s$x, c(NA, 0.5 + 0:10))
  expect_equal(s$count, rep(2, length(x)))
})

test_that("z affects sums, but not counts", {
  x <- c(NA, 0:10)
  z <- 0:11
  s <- summarise1d(x, z, summary = "sum", binwidth = 1)

  expect_equal(s$count, rep(1, length(x)))
  expect_equal(s$sum, z)
})
