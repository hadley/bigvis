#' Fast 1d kernel density estimation on regularly binned data.
#' 
#' Currently only provides the normal kernel, but extension to other kernels
#' is straightforward (but ignore because not that important). The methods
#' used in this function are adapted from the binned kernel density 
#' estimation functions in MP Wand's "KernSmooth" package, and are fast
#' because they work with binned data and use the discrete fast fourier
#' algorithm for efficient convolution of the data and kernel.
#'
#' @param bindata Output from \code{\link{bin_1d}} or otherwise - needs
#'   to be in the same format. 
#' @param bandwidth smoothing bandwith - higher number = more smoothing.
#' @export
#' @examples
#' x <- rnorm(1000)
#' bin <- bin_1d(data.frame(x), "x", 0.01)
#' dens <- density_1d(bin, 0.5)
#' plot(dens, type = "l")
density_1d <- function(bindata, bandwidth) { 
  rng <- bindata$range
  nbin <- bindata$nbin
  
  # Ignore missings in smoothing process
  n_missing <- bindata$count[nbin + 1]
  counts <- bindata$count[-(nbin) + 1]
  n <- sum(counts)
  
  binwidth <- (rng[2] - rng[1]) / nbin
  delta <- binwidth / bandwidth
  L <- floor(1 / delta) * 4
  if (L == 0) {
    warning("Bandwidth too small for the number of bins")
  }
  Lvector <- 0:L
  
  # Extend range by 4 bandwidths, padding with 0's
  rng <- rng + c(-1, 1) * 4 * bandwidth
  
  # Convert counts to proportions and scale
  counts <- counts / n
  counts <- c(rep(0, L), counts, rep(0, L))
  n_out <- length(counts)
    
  # Generate smoothing half-kernel
  kappa <- dnorm(Lvector * delta) / (n * bandwidth)
  # # Check that sum of kappa close to 0.5
  # weight <- sum(kappa) * delta * n 
  # stopifnot(abs(weight - 0.5) < 1e-3)
  
  # Zero pad kernel and data to avoid wrap around effects and ensure 
  # size is power of 2
  P <- 2 ^ ceiling(log2(nbin + L))
  kappa2 <- c(kappa, rep(0, 2 * P - 2 * length(kappa)), rev(kappa))
  kappa2 <- kappa2 / sum(kappa2)
  
  counts2 <- c(counts, rep(0, 2 * P - n_out))
  
  # Use dFFT to convolve data and kernel
  kords <- fft(fft(counts2) * Conj(fft(kappa2)), inverse = TRUE) /
    length(counts2)
  
  y <- pmax.int(0, Re(kords)[seq_along(counts)]) / binwidth
  x <- seq(rng[1], rng[2], length = n_out)

  # Adjust density for missing values
  if (n_missing > 0) {
     y <- y * (1 - n_missing / n)
  }
  
  list(x = x, y = y)
}

