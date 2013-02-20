context("Weighted statistics")

test_that("weighted.var agrees with var when weights = 1", {
  samples <- replicate(20, runif(100), simplify = FALSE)

  var <- sapply(samples, var)
  wvar <- sapply(samples, weighted.var, w = rep(1, 100))

  expect_equal(wvar, var)
})

test_that("weighted.var agrees with var on repeated vector", {
  samples <- replicate(20, runif(100), simplify = FALSE)
  w <- rep(1:2, 50)
  samples_ex <- lapply(samples, rep, times = w)

  var <- sapply(samples_ex, var)
  wvar <- sapply(samples, weighted.var, w = w)

  expect_equal(wvar, var)
})

test_that("weighed.quantile agrees with quantile on repeated vector", {
  samples <- replicate(20, runif(100), simplify = FALSE)
  w <- rep(1:2, 50)
  samples_ex <- lapply(samples, rep, times = w)

  quant <- sapply(samples_ex, quantile, probs = 0.325, names = FALSE)
  wquant <- sapply(samples, weighted.quantile, w = w, probs = 0.325)

  expect_equal(quant, wquant)
})
