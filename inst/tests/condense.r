context("Summary: 1d")

test_that("summarise_1d counts small vectors accurately", {
  x <- c(NA, 0:10)
  s1 <- condense(grouped(x, 1, -0.5), summary = "count")
  # Pathological origin: need to add extra bin on end, because they're
  # right open, left closed
  s2 <- condense(grouped(x, 1, 0), summary = "count")

  expect_equal(s1$x, c(NA, 0:10))
  expect_equal(s2$x, c(NA, 0:10 + 0.5))

  expect_equal(s1$count, rep(1, length(x)))
  expect_equal(s2$count, rep(1, length(x)))
})

test_that("weights modify counts", {
  x <- c(NA, 0:10)
  w <- rep(2, length(x))
  s <- condense(grouped(x, 1), w = w, summary = "count")

  expect_equal(s$x, c(NA, 0:10))
  expect_equal(s$count, rep(2, length(x)))
})

test_that("z affects sums, but not counts", {
  x <- c(NA, 0:10)
  z <- 0:11
  s <- condense(grouped(x, 1), z, summary = "sum")

  expect_equal(s$count, rep(1, length(x)))
  expect_equal(s$sum, z)
})

test_that("summarise1d computes breaks correctly", {

})
