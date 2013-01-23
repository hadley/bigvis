context("Summary: moments")

count2 <- function(x) compute_moments(x)[1]
mean2 <- function(x) compute_moments(x)[2]
sd2 <- function(x) compute_moments(x)[3]

test_that("count agrees with length", {
  expect_equal(count2(1:10), 10)
  expect_equal(count2(5), 1)
  expect_equal(count2(numeric()), 0)
})

test_that("mean agree with base::mean", {
  expect_equal(mean2(1:10), mean(1:10))

  x <- runif(1e6)
  expect_equal(mean2(x), mean(x))
})

test_that("missing values are ignored", {
  x <- c(NA, 5, 5)
  expect_equal(count2(x), 2)
  expect_equal(mean2(x), 5)
})

test_that("standard deviation agrees with sd", {
  expect_equal(sd2(1:10), sd(1:10))

  x <- runif(1e6)
  expect_equal(sd2(x), sd(x))
})

test_that("summary statistics of zero length input are NaN", {
  expect_equal(compute_moments(numeric()), c(0, NaN, NaN))
})
