# load library
library(testthat)
library(plyr)
library(MASS)

source("bin_2d_NA.R")
source("density_2d.R")
source("myImagePlot.R")
# normal densities

x <- rnorm(1000, 0, 1) 
y <- rnorm(1000, 0, 1)


x <- c(-10,10,30) 
y <- c(-10,15,35)
df <- data.frame(cbind(x,y))

# density_1d
binx <- bin_2d(df, "x", "y", c(0.1,0.1))
binx <- bin_2d(df, "x", "y", c(0.083, 0.1))

bindata <- binx
# set bandwidth
bandwidth <- c(1, 1)


dens1 <- density_2d(binx, bandwidth)

dens2 <- kde2d(x, y, h = c(1,1), n = 400, lims = c(range(x), range(y)))

mse <- mean(sum(dens1$z - dens2$z)^2)

image(dens1, useRaster = TRUE)
image(dens2, useRaster = TRUE)





myImagePlot(dens1$z)
myImagePlot(dens2$z)


myImagePlot(z)
