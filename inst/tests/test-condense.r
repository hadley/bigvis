context("Condense")

test_that("condense counts small vectors accurately", {
  x <- c(NA, 0:10)
  s1 <- condense(bin(x, 1, -0.5), summary = "count")
  # Pathological origin: need to add extra bin on end, because they're
  # right open, left closed
  s2 <- condense(bin(x, 1, 0), summary = "count")

  expect_equivalent(s1$x, c(NA, 0:10))
  expect_equivalent(s2$x, c(NA, 0:10 + 0.5))

  expect_equal(s1$.count, rep(1, length(x)))
  expect_equal(s2$.count, rep(1, length(x)))
})

test_that("weights modify counts", {
  x <- c(NA, 0:10)
  w <- rep(2, length(x))
  s <- condense(bin(x, 1), w = w, summary = "count")

  expect_equivalent(s$x, c(NA, 0:10))
  expect_equal(s$.count, rep(2, length(x)))
})

test_that("z affects sums, but not counts", {
  x <- c(NA, 0:10)
  z <- 0:11
  s <- condense(bin(x, 1), z = z, summary = "sum")

  expect_equal(s$.count, rep(1, length(x)))
  expect_equal(s$.sum, z)
})

test_that("drop = FALSE and drop = TRUE results agree", {
  x <- runif(1e3)
  y <- x + runif(1e3, -0.2, 0.2)
  z <- rnorm(1e3, x)

  gx <- bin(x, 0.1)
  gy <- bin(y, 0.1)

  count1 <- condense(gx, gy, summary = "count", drop = TRUE)
  expect_equal(sum(count1$.count == 0), 0)

  count2 <- condense(gx, gy, summary = "count", drop = FALSE)
  expect_equivalent(count1, count2[count2$.count != 0, ])
})

# 2d tests ---------------------------------------------------------------------

test_that("grid counted accurately", {
  # expand.grid orders in opposite way to bigvis
  grid <- expand.grid(y = c(NA, 1:2), x = c(NA, 1:2))
  s <- condense(bin(grid$x, 1), bin(grid$y, 1))

  expect_equal(s$.count, rep(1, nrow(grid)))
  expect_equivalent(s$grid.x, grid$x)
  expect_equivalent(s$grid.y, grid$y)
})

test_that("diagonal counted correctly", {
  df <- data.frame(x = c(NA, 1:2), y = c(NA, 1:2))
  s <- condense(bin(df$x, 1), bin(df$y, 1))

  expect_equal(nrow(s), nrow(df))
  expect_equal(s$df.x, s$df.y)
})

test_that("random data doesn't crash", {
  x <- runif(1e3, 8, 4963)
  y <- runif(1e3, 1e-2, 1e3)

  gx <- bin(x, 10)
  gy <- bin(y, 10)

  condense(gx, gy)
})
