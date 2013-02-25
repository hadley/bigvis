context("Condense")

test_that("condense counts small vectors accurately", {
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

# 2d tests ---------------------------------------------------------------------

test_that("grid counted accurately", {
  grid <- expand.grid(x = c(NA, 1:2), y = c(NA, 1:2))
  s <- condense(list(grouped(grid$x, 1), grouped(grid$y, 1)))

  expect_equal(s$count, rep(1, nrow(grid)))
  expect_equal(s$grid.x, grid$x)
  expect_equal(s$grid.y, grid$y)
})

test_that("diagonal counted correctly", {
  df <- data.frame(x = c(NA, 1:2), y = c(NA, 1:2))
  s <- condense(list(grouped(df$x, 1), grouped(df$y, 1)))
  s <- s[s$count == 1, ]

  expect_equal(nrow(s), nrow(df))
  expect_equal(s$df.x, s$df.y)
})

test_that("random data doesn't crash", {
  x <- runif(1e3, 8, 4963)
  y <- runif(1e3, 1e-2, 1e3)

  gx <- grouped(x, 10)
  gy <- grouped(y, 10)

  condense(list(gx, gy))
})
