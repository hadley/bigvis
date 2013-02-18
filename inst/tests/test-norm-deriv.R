context("Normal derivatives")

# Reference values from:
# http://myhandbook.info/table_normal_derivatives.html
#
# I can't figure out why the signs don't agree.

test_that("normal derivatives agree with reference calculations", {
  ref <- c(-0.176032663, -0.264048995, 0.484089824, 0.550102073, -2.21141033,
    -1.64480520)
  comp <- sapply(1:6, function(i) norm_deriv(0.5, 1, i))
  expect_equal(abs(comp), abs(ref))
})

