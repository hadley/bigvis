context("Binned vectors")

bv <- function(...) {
  BigVis$BinnedVectors$new(list(...))
}
g <- grouped(1:10, 1)
bvs <- bv(g, g)
bvs$bin(c(3, 5))
bvs$unbin(58)

test_that("nbins correct", {
  g <- grouped(1:10, 1)
  expect_equal(bv(g)$nbins(), 11)
  expect_equal(bv(g, g)$nbins(), 11 ^ 2)
  expect_equal(bv(g, g, g)$nbins(), 11 ^ 3)
})

test_that("bin and unbin are symmetric", {
  g <- grouped(1:10, 1)
  bvs <- bv(g, g)

  for(x in 1:10) {
    for(y in 1:10) {
      bin <- bvs$bin(c(x, y))
      expect_equal(bvs$unbin(bin), c(x, y), expected.label = c(x, y))
    }
  }
})
