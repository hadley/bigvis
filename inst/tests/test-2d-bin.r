library(plyr)
library(testthat)
source("bin_2d_NA.R")
# generate random data
data <- data.frame(cbind(rnorm(100,0,100),rnorm(100,0,10)))
names(data) <-  c("x","y")
bin = c(30,10)
x = "x"
y = "y"
#bin_2d(data,x,y,bin)
data_NA = data
data_NA$x[sample(100,5)] = NA
data_NA$y[sample(100,5)] = NA

# convert to xdf file
write.csv(data_NA, "data_NA_2d.csv", row.names = FALSE)
bin_2d(data_NA, x, y, bin)

rxTextToXdf(inFile = "data_NA_2d.csv", outFile = "dataNA_2d", stringsAsFactors = TRUE, reportProgress = 0, overwrite = TRUE)


# compare results of bin_1d("dataNA", "x", bin) and bin_1d(data_NA, "x", bin)
bin.xdf <- bin_2d("dataNA_2d", x, y, bin)
bin.df <- bin_2d(data_NA, x, y, bin)

test_that("2d binning works the same for data frame and xdf file",{
	all.equal(bin.df$Counts,bin.xdf$Counts)
	all.equal(bin.df$Breaks, bin.xdf$Breaks, tol = 1e-7)
	all.equal(bin.df$Values, bin.xdf$Values, tol = 1e-7)		
})


