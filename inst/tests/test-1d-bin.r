library(plyr)
library(testthat)
source("bin_1d_NA.R")
# generate random data
data <- data.frame(rnorm(100,0,100))
names(data) <-  "x"
bin <- 10
x <- "x"
data_NA = data
data_NA$x[sample(100,5)] = NA
# convert to xdf file
write.table(data_NA, "data_NA.csv", row.names = FALSE)
rxTextToXdf(inFile = "data_NA.csv", outFile = "dataNA", reportProgress = 0, stringsAsFactors = TRUE, overwrite = TRUE)


# compare results of bin_1d("dataNA", "x", bin) and bin_1d(data_NA, "x", bin)

bin.xdf <- bin_1d("dataNA", "x", bin)
bin.df <- bin_1d(data_NA, "x", bin)
test_that("1d binning works the same for data frame and xdf file",{
    all.equal(bin.df$Counts,bin.xdf$Counts)
    all.equal(bin.df$Breaks, bin.xdf$Breaks, tol = 1e-7)
    all.equal(bin.df$Values, bin.xdf$Values, tol = 1e-7)    
})


test_that("bin_1d equivalent to table", {
  x <- sample(100, 1000, rep = T)
  tbl <- c(table(x)[], 0)
  bin <- bin_1d(data.frame(x), "x", 1)
  expect_that(tbl, equals(bin$Counts))  
})



