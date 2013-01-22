context("1d binning")

# test_that("binning integers equivalent to table", {
#   x <- c(0, 100, sample(100, 1000, rep = TRUE))
#   bin <- bin_1d(data.frame(x), "x", 1)
#   expect_that(bin$range, equals(c(0, 100)))

#   tbl <- c(table(x)[], 0)
#   expect_that(tbl, equals(bin$count))
# })

# test_that("binning reals equivalent to findInterval + tabulate", {
#   x <- c(-10, 10, rnorm(1000))
#   bin <- bin_1d(data.frame(x), "x", 1)
#   expect_that(bin$range, equals(c(-10, 10)))

#   bins <- findInterval(x, seq(-10, 10))
#   tbl <- c(tabulate(bins), 0)
#   expect_that(tbl, equals(bin$count))
# })

# test_that("missing values counted correct", {
#   x <- 1:10
#   bin <- bin_1d(data.frame(x), "x", 1)
#   expect_that(bin$count[[11]], equals(0))

#   x <- c(1:10, NA)
#   bin <- bin_1d(data.frame(x), "x", 1)
#   expect_that(bin$count[[11]], equals(1))
# })
