context("1d smoothing")

data <- data.frame(x = rnorm(100,0,100))
compare_density <- function(x, binwidth = .01, bandwidth = .1) {
  df <- data.frame(x)
  binx <- bin_1d(df, "x", binwidth)
  dens1 <- density_1d(binx, bandwidth)

  dens2 <- density(x, bw = bandwidth)

  d1 <- approxfun(dens1$x, dens1$y)
  d2 <- approxfun(dens2$x, dens2$y)

  grid <- seq(min(dens2$x), max(dens2$x), length = 500)
  mse <- mean((d1(grid) - d2(grid)) ^ 2)

  list(x = grid, y1 = d1(grid), y2 = d2(grid), mse = mse)
}

test_that("density1d similar to base::density", {
  norm <- compare_density(rnorm(1e3))
  unif <- compare_density(runif(1e3))
  
  expect_true(norm$mse < 1e-3)
  expect_true(unif$mse < 1e-3)
})

# Graphical exploration of errors
if (FALSE) {
  norm <- compare_density(rnorm(1e4))
  plot(norm$x, norm$y1, type = "l")
  lines(norm$x, norm$y2, col = "blue")

  plot(norm$x, norm$y1 - norm$y2, type = "l")
  abline(h = 0, col = "grey50")
  rug(x)
}

# if (require("revoScaleR", quiet = TRUE)) {
#   # test on airline data
#   ADS_1e6 <- rxReadXdf(file = "AirlineData87to08", varsToKeep = c("ArrDelay","CRSDepTime","DayOfWeek"), startRow = 9000000, numRows = 1000000)
#   write.table(ADS_1e6,"ADS_1e6.csv",sep = ",", row = F)
#   rxTextToXdf(inFile = "ADS_1e6.csv", outFile = "ADS_1e6", stringsAsFactors = TRUE, overwrite = TRUE)    
# 
#   info =  rxGetInfoXdf("ADS_1e6", getVarInfo = TRUE)
#   bin2 = (info$varInfo[[1]]$high - info$varInfo[[1]]$low)/100
#   bin100 <- bin_1d("ADS_1e6",1,bin2)
#   smooth_100 <- density_1d(bin100,50)
#   smooth_100 <- density_1d(bin100,200)
#   smooth_100 <- density_1d(bin100,15)
# 
#   write.csv(smooth_100, "smooth100.csv", row.names = F)
#   write.csv(smooth_100, "smooth100_2.csv", row.names = F)
#   write.csv(smooth_100, "smooth100_3.csv", row.names = F)
# 
#   hist.dens <- hist(na.omit(ADS_1e6$ArrDelay),breaks = dim(smooth_100)[1],  plot = F)
#   hist.dens <- cbind(hist.dens$mids, hist.dens$density)
#   write.csv(hist.dens, "hist_100.csv", row.names = F)
# 
#   #scp -r yuehu@173.203.91.117:smooth100.csv ~/Desktop/
#   #scp -r yuehu@173.203.91.117:hist100.csv ~/Desktop/
#   ads<-read.csv("ADS_1e6.csv", stringsAsFactors = F)
#   smooth100 <- read.csv("smooth100.csv", stringsAsFactors = F)
#   smooth100_2 <- read.csv("smooth100_2.csv", stringsAsFactors = F)
#   smooth100_3 <- read.csv("smooth100_3.csv", stringsAsFactors = F)
# 
#   hist100 <- read.csv("hist100.csv", stringsAsFactors = F)
#   hist(na.omit(ads$ArrDelay), breaks = 100, freq = F, main = "Histogram of ArrDelay")
#   lines(hist100$V1, hist100$V2, col = "red", type = "l", xlab = "x", ylab = "density")
#   lines(smooth100$x, smooth100$y, type = "l", col = "blue")
#   lines(smooth100_2$x, smooth100_2$y, type = "l", col = "yellow")
#   lines(smooth100_3$x, smooth100_3$y, type = "l", col = "green")
# 
#   legend(250, 0.015,c("density estimated by hist", "my density: bw = 50", "my density: bw = 20", "my density: bw = 15"), col=c("red","blue","yellow", "green"),lty=c(2,1))
#   
# }
