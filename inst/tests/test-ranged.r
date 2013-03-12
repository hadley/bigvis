context("Ranged")

test_that("range attribute lost when modified", {
  x <- ranged(10:1)
  expect_equal(max(x), 10)


  x[1] <- 1
  expect_equal(max(x), 9)
  expect_equal(attr(x, "range"), NULL)
})
