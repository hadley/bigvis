context("Stats")

test_that("linear regression recovers slope & intercept if no errors", {
  x <- 1:10
  w <- rep(1, 10)

  expect_equal(regress(x, x * 2, w), c(0, 2))
  expect_equal(regress(x, x * -2, w), c(0, -2))
  expect_equal(regress(x, x * -2 + 5, w), c(5, -2))
  expect_equal(regress(x, x * -2 + -5, w), c(-5, -2))
})

simpleLm <- function(x, y, w) {
  unname(coef(lm(y ~ x, weights = w)))
}

test_that("linear regression matches lm", {
  x <- 1:10
  y <- 10 + x * 2 + rnorm(10)
  w <- rep(1, 10)

  expect_equal(regress(x, y, w), simpleLm(x, y, w))
})

test_that("linear regression matches lm with weights", {
  x <- 1:10
  y <- 10 + x * 2 + rnorm(10)
  w <- runif(10)

  expect_equal(regress(x, y, w), simpleLm(x, y, w))
})

test_that("robust regression effectively removes outlier", {
  x <- 1:10
  y <- 10 + x * 2 + c(rep(0, 9), 10)
  w <- rep(1, 10)

  expect_equal(regress_robust(x, y, w, 10), c(10, 2))
})



