# load library
library(testthat)
library(plyr)
library(MASS)

source("bin_1d_NA.R")
source("density_1d.R")

# normal densities

x <- rnorm(1000, 0, 2) 
x <- c(-10, 15,30)

df <- data.frame(x)

# density_1d
binx <- bin_1d(df, "x", .1)
bindata <- binx
# set bandwidth
bandwidth <- 1


dens1 <- density_1d(binx, bandwidth)
	
dens2 <- density(x, bw = bandwidth)

dens3 <- bkde(x,bandwidth = bandwidth)

d1 <- approxfun(dens1$x, dens1$y)
d2 <- approxfun(dens2$x, dens2$y)
d3 <- approxfun(dens3$x, dens3$y)
grid <- seq(min(dens2$x), max(dens2$x), length = 500)

plot(grid, d1(grid), type = "l", col = "red")
lines(grid, d2(grid), col = "blue")
lines(grid, d3(grid), col = "green")
legend(0, 0, c("R density function", "density_1d", "kernsmooth"), col=c("red","blue","greeb"))

plot(grid, d1(grid) - d2(grid), type = "l")

# mean squared difference
mean((d1(grid) - d2(grid))^2)

compare_density <- function(x, binwidth = .1, bandwidth = 1) {
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
compare_density(c(-10, 15,30), 0.1, 1)

compare_density2 <- function(x, binwidth = .1, bandwidth = 1) {
	df <- data.frame(x)
	binx <- bin_1d(df, "x", binwidth)
	dens1 <- density_1d(binx, bandwidth)
	dens3 <- bkde(x,bandwidth = bandwidth)

	d1 <- approxfun(dens1$x, dens1$y)
	d3 <- approxfun(dens3$x, dens3$y)

	grid <- seq(min(dens3$x), max(dens3$x), length = 500)
	mse <- mean((d1(grid) - d3(grid)) ^ 2)
	
	list(x = grid, y1 = d1(grid), y3 = d3(grid), mse = mse)
}
compare_density2(c(-10, 15,30), 0.1, 1)

compare_density3 <- function(x, binwidth = .1, bandwidth = 1) {
	df <- data.frame(x)
	dens2 <- density(x, bw = bandwidth)
    dens3 <- bkde(x,bandwidth = bandwidth)

	d2 <- approxfun(dens2$x, dens2$y)
	d3 <- approxfun(dens3$x, dens3$y)

	grid <- seq(min(dens2$x), max(dens2$x), length = 500)
	mse <- mean((d2(grid) - d3(grid)) ^ 2)
	
	list(x = grid, y2 = d2(grid), y3 = d3(grid), mse = mse)
}
compare_density3(x, 0.1, 1)


x <- runif(1000)
x <- rt(1000,df =200)
cd <- compare_density3(x, 0.1, .1)
plot(cd$x, cd$y2 - cd$y3, type = "l")
abline(h = 0, col = "grey50")
rug(x)

plot(cd$x, cd$y1, type = "l", col = "red")
lines(cd$x, cd$y2, col = "blue")
legend(2, 0.3, c("R density function", "density_1d"), col=c("red","blue"))
