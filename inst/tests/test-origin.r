context("Origin")

test_that("origins close to zero rounded to zero" ,{
  expect_equal(find_origin(c(0.01, 1000)), 0)
  expect_equal(find_origin(c(10, 1e6)), 0)
})

test_that("origins rounded down by binwidth", {
  expect_equal(find_origin(c(1, 10), 1), 1)
  expect_equal(find_origin(c(1, 10), 2), 0)

  expect_equal(find_origin(c(5, 10), 2), 4)
  expect_equal(find_origin(c(5, 10), 5), 5)
})

test_that("integers have origin offset by 0.5", {
  expect_equal(find_origin(c(1L, 10L), 1), 0.5)

  expect_equal(find_origin(c(5L, 10L), 2), 3.5)
  expect_equal(find_origin(c(5L, 10L), 5), 4.5)
})
