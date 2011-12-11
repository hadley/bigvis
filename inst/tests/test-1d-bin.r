context("1d binning")
# compare results of bin_1d("dataNA", "x", bin) and bin_1d(data_NA, "x", bin)

test_that("binning integers equivalent to table", {
  x <- c(0, 100, sample(100, 1000, rep = TRUE))
  bin <- bin_1d(data.frame(x), "x", 1)
  expect_that(bin$range, equals(c(0, 100)))

  tbl <- c(table(x)[], 0)
  expect_that(tbl, equals(bin$count))
})

test_that("binning reals equivalent to findInterval + tabulate", {
  x <- c(-10, 10, rnorm(1000))
  bin <- bin_1d(data.frame(x), "x", 1)
  expect_that(bin$range, equals(c(-10, 10)))
  
  bins <- findInterval(x, seq(-10, 10))
  tbl <- c(tabulate(bins), 0)
  expect_that(tbl, equals(bin$count))  
})

test_that("missing values counted correct", {
  x <- 1:10
  bin <- bin_1d(data.frame(x), "x", 1)
  expect_that(bin$count[[11]], equals(0))
  
  x <- c(1:10, NA)
  bin <- bin_1d(data.frame(x), "x", 1)
  expect_that(bin$count[[11]], equals(1))
})

# Comparisons between data frames and xdfs -----------------------------------

if (require("revoScaleR", quiet = TRUE)) {
  
  # generate random data
  df1 <- data.frame(x = rnorm(100,0,100))
  bin <- 10
  x <- "x"
  data_NA = data
  data_NA$x[sample(100,5)] = NA
  
  
  # convert to xdf file
  write.table(data_NA, "data_NA.csv", row.names = FALSE)
  rxTextToXdf(inFile = "data_NA.csv", outFile = "dataNA.xdf", 
    reportProgress = 0, stringsAsFactors = TRUE, overwrite = TRUE)

  test_that("1d binning works the same for data frame and xdf file",{
    bin.xdf <- bin_1d("dataNA.xdf", "x", bin)
    bin.df <- bin_1d(data_NA, "x", bin)

    all.equal(bin.df$Counts,bin.xdf$Counts)
    all.equal(bin.df$Breaks, bin.xdf$Breaks, tol = 1e-7)
    all.equal(bin.df$Values, bin.xdf$Values, tol = 1e-7)
  })

  
  unlink("data_NA.csv")
  unlink("data_NA.xdf")
}

