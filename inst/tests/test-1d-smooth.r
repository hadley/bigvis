# generate random data
data <- data.frame(rnorm(100,0,100))
names(data) <-  "x"
bin <- 10
x <- "x"
data_NA = data
data_NA$x[sample(100,5)] = NA
# convert to xdf file
#write.table(data_NA, "data_NA.csv", row.names = FALSE)
#rxTextToXdf(inFile = "data_NA.csv", outFile = "dataNA", reportProgress = 0, stringsAsFactors = TRUE, overwrite = TRUE)

# get binned data
#bin.xdf <- bin_1d("dataNA", "x", bin)
bin.df <- bin_1d(data_NA, "x", bin)
# 
# dens <- density_1d(bin.df, 100)
# dens2 <- density_1d(bin.df, 10)
# 
# hist(na.omit(data_NA$x), breaks = length(dens$x), freq = FALSE, main = "histogram of x", xlab = "x")
# hist.dens <- hist(na.omit(data_NA$x),breaks = length(dens$x),  plot = F)
# lines(hist.dens$mids, hist.dens$density, col = "red")
# lines(dens$x, dens$y, col = "blue")
# lines(dens2$x, dens2$y, col = "green")
# legend(-300, 0.006,c("density estimated by hist","my density: bw = 100","my density: bw = 10"), col=c("red","blue","green"),lty=c(2,1))
# 
# # normal densities
# 
# x <- rnorm(1000, 0, 2) 
# x <- c(-10, 15,30)
# 
# df <- data.frame(x)
# 
# # density_1d
# binx <- bin_1d(df, "x", .1)
# bindata <- binx
# # set bandwidth
# my.x.density <- density_1d(binx, 1)
# plot(my.x.density$x, my.x.density$y, col='blue', type = "l")
# 
# 
# # density1 <- density_1d(binx, 1)
# # expect_equal(sum(density1$y), 1)
# 
# # R density function esimates   
# x.density <- density(x, bw = 1)
# 
# 
# grid <- seq(min(x.density$x), max(x.density$x), length = 500)
# 
# plot(grid, d1(grid), type = "l", col = "red")
# lines(grid, d2(grid), col = "blue")
# legend(0, 0, c("R density function", "density_1d"), col=c("red","blue"))
# 
# plot(grid, d1(grid) - d2(grid), type = "l")
# 
# # mean squared difference
# mean((d1(grid) - d2(grid))^2)
# 
# compare_density <- function(x, binwidth = .1, bandwidth = 1) {
#     df <- data.frame(x)
#     binx <- bin_1d(df, "x", binwidth)
#     dens1 <- density_1d(binx, bandwidth)
# 
#     dens2 <- density(x, bw = bandwidth)
# 
#     d1 <- approxfun(dens1$x, dens1$y)
#     d2 <- approxfun(dens2$x, dens2$y)
# 
#     grid <- seq(min(dens2$x), max(dens2$x), length = 500)
#     mse <- mean((d1(grid) - d2(grid)) ^ 2)
# 
#     list(x = grid, y1 = d1(grid), y2 = d2(grid), mse = mse)
# }
# compare_density(c(-10, 15,30), 0.1, 1)
# 
# x <- runif(1000)
# cd <- compare_density(x, 0.1, .1)
# plot(cd$x, cd$y1 - cd$y2, type = "l")
# abline(h = 0, col = "grey50")
# rug(x)
# 
# 
# 
# if (require("revoScaleR", quiet = TRUE)) {
#   # test on airline data
#   ADS_1e6 <- rxReadXdf(file = "AirlineData87to08", varsToKeep = c("ArrDelay","CRSDepTime","DayOfWeek"), startRow = 9000000, numRows = 1000000)
#   write.table(ADS_1e6,"ADS_1e6.csv",sep = ",", row = F)
#   rxTextToXdf(inFile = "ADS_1e6.csv", outFile = "ADS_1e6", stringsAsFactors = TRUE, overwrite = TRUE)    
# 
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
# 
#   
# }
# 
