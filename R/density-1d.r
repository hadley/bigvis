#Binned kernel density estimation in one dimension (using normal kernels).
#This function takes the output object from the "bin_1d" function. The other 
#variable, "bandwidth", is a smoothing parameter. note that for low numbers
#of bins, a higher smoothing parameter is needed. 
#The methods used in this function are adapted from the binned kernel density 
#estimation functions in MP Wand's "KernSmooth" package.

density_1d <- function(bindata, bandwidth) { 
  a <- bindata$Values[["low"]]
  b <- bindata$Values[["high"]]
  
  # Drop missings out
  if (bindata$Counts[length(bindata$Counts)] != 0){
      counts <- bindata$Counts[-length(bindata$Counts)]
      nbin <- length(counts)
      n <- sum(counts) #total number of observations
  } else {
        nbin <- length(bindata$Counts)
      n <- sum(bindata$Counts) #total number of observations
      counts <- bindata$Counts
  }
  
  binwidth <- (b - a) / nbin
  delta <- binwidth / bandwidth
  L <- floor(1 / delta) * 4
  
  # Extend range by 4 bandwidths, padding with 0's
  a <- a - 4 * bandwidth
  b <- b + 4 * bandwidth

  counts <- counts / n
  counts <- c(rep(0, L), counts, rep(0, L))
    
  # Discretise kernel (kernel weights)
  if (L == 0) warning("This bandwidth is too small for the number of bins")
  Lvector <- 0:L
  kappa <- dnorm(Lvector * delta) / (n * bandwidth) #normal kernel weight
  # Ensure that kappa sums to 0.5
  weight <- sum(kappa) * delta * n 
  #stopifnot(abs(weight - 0.5) < 0.1)

  P <- 2 ^ ceiling(log2(nbin + L))
  kappa2 <- c(kappa, rep(0, 2 * P - 2 * length(kappa)), rev(kappa))
  kappa2 <- kappa2 / sum(kappa2)
  
  #Zero-padding to avoid wrap-around effects.
  counts2 <- c(counts, rep(0, 2 * P - length(counts)))
  
  #Now we use the Fast Fourier Transform to compute the kernel densities.
  kords <- fft(fft(counts2) * Conj(fft(kappa2)), inverse = TRUE) / length(counts2)

  y <- pmax.int(0, Re(kords)[seq_along(counts)]) / binwidth
  x <- seq(a, b, length = length(counts))
  #browser()
  # Adjust density for missing values
  if (bindata$Counts[length(bindata$Counts)] != 0){
     p_missing <- 1 - sum(counts) / sum(bindata$Counts)
     # y <- c(y * (1 - p_missing), p_missing)
     y = y * (1 - p_missing)
  }
  #list(density = y, breaks = bindata$breaks)
  list("x" = x, "y" = y)
}

