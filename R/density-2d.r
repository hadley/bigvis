#Binned kernel density estimation in two dimensions (using normal kernels).
#This function takes the output object from the "bin_2d" function. The other 
#variable, "bandwidth", is a smoothing parameter. note that for low numbers
#of bins, a higher smoothing parameter is needed. 
#The methods used in this function are adapted from the binned kernel density 
#estimation functions in MP Wand's "KernSmooth" package.

density_2d <- function(bindata, bandwidth) {
  # if (is.vector(bandwidth)) {
  #     bandwidth <- diag(bandwidth)
  # }
    
  a <- bindata$Values[["low"]]
  b <- bindata$Values[["high"]]
  counts <- bindata$Counts
  if (length(bandwidth) == 1) bandwidth <- rep(bandwidth, 2)
  nbin <- c(dim(counts)[1], dim(counts)[2])

  binwidth <- (b - a) / nbin
  delta <- binwidth / bandwidth
  L <- floor(1 / delta) * 4
  if (min(L) == 0) warning("Bandwidth is too small")

  # extend range by 4 bandwidths, pad counts with 0's
  a <- a - 4 * max(bandwidth)
  b <- b + 4 * max(bandwidth)

  n <- sum(bindata$Counts)
  counts <- counts / n
    
  # Padding size - matrix needs to be big enough to hold both kernel and data counts
  P <- 2^(ceiling(log2(pmax(L, nbin))) + 1)

  # Set up kernel matrix
  kappa <- list(0, 0)
  for (i in 1:2) {
      grid <- seq(-P[i]/2, P[i]/2, length = P[i]) * bandwidth[i]
    kappa[[i]] <- dnorm(grid * delta[i]) / (n * bandwidth[i])
    #weight <- sum(kappa[[i]]) * delta[i] * n
    #kappa[[i]] <- kappa[[i]]/weight
  }
  kappa <- kappa[[1]] %*% t(kappa[[2]])
  
  # library(mvtnorm)
  # kappa <- dmvnorm(  , mean = 0, sigma = bandwidth)
  
  # Zero-padded version of counts to avoid wrap-around effects during fft
  padded <- matrix(0, P[1], P[2])  
  padded[1:nbin[1], 1:nbin[2]] <- counts
  # padded[1:nbin[1] + floor(P[1] - nbin[1])/2, 1:nbin[2] + floor(P[2] - nbin[2])/2] <- counts
  # image(padded, useRaster = T)

  #Now we use the Fast Fourier Transform to calculate kernel density.

  z <- Re(fft(fft(padded) * Conj(fft(kappa)), inverse = TRUE)/(P[1] * P[2]))
  z <- z[nrow(z):1, ncol(z):1]
  # [1:nbin[1], 1:nbin[2]]
  # image(z, useRaster = T)
  # 
  browser()
  
  #To ensure that there are no negative values from floating point errors,
  #we multiply z by a matrix of 1's where z had positive values and 0's where 
  #z had negative values.  
  x <- seq(a[1], b[1], length = nbin[1])
  y <- seq(a[2], b[2], length = nbin[2])
  z <- matrix(pmax.int(0, z), nrow(z), ncol(z)) / (binwidth[1] * binwidth[2])

  list("x" = x, "y" = y, "z" = z)
}

#Note that to plot with most R plotting functions, you must turn z into a vector,
#with the x's and y's expanded.


# read 2d convolution
# install.packages(, type = "source")
