#' Fast 1d kernel density estimation on regularly binned data.
#' 
#' Currently only provides the normal kernel, but extension to other kernels
#' is straightforward (but ignored because they don't make that much
#' difference to the final result). 
#'
#' The methods used in this function are adapted from the ideas of binned
#' kernel density in MP Wand's "KernSmooth" package, but rather than using
#' the discrete discrete fast fourier algorithm, they do convolution using 
#' C-level loops. This is more efficient when the matrices to be convolved
#' are rather different sizes.
#'
#' @param counts Output from \code{\link{bin_1d}} or otherwise - needs
#'   to be in the same format. 
#' @param bandwidth smoothing bandwith - higher number = more smoothing.
#' @export
#' @examples
#' bin <- bin_nd(mtcars, "mpg", 0.01)
#' dens <- density_1d(bin, 0.5)
#' plot(dens)
#' lines(bin, col = "red")
#' 
#' dens <- density_1d(bin, 0.25)
#' plot(dens)
#' lines(bin, col = "red")
density_1d <- function(counts, bandwidth) { 
  stopifnot(is.binned_summary(counts)) # must come from bin_nd
  stopifnot(length(dim(counts)) == 1)  # must be 1d
  
  binwidth <- counts$binwidth[[1]]
  stopifnot(!is.na(binwidth))   # must be continuous

  # # Ignore missings in smoothing process
  # n_missing <- bindata$count[nbin + 1]
  # counts <- bindata$count[-(nbin) + 1]
  # n <- sum(counts)
  
  # Generate the kernel
  # length of kernel (in bins) needed to get 4 standard deviations
  l <- trunc(bandwidth / binwidth) * 4L
  grid <- seq(-4, 4, length = 2L * l + 1L)
  kernel <- dnorm(grid, sd = bandwidth)

  # Smooth, by convolving data with kernel
  smooth <- array(convolve_1d(counts$data, kernel))
  
  centers <- counts$centers[[1]]
  n <- length(centers)
  centers <- c(
    centers[1] - seq.int(l, 1) * binwidth,
    centers, 
    centers[n] + seq.int(1, l) * binwidth
  )
  
  counts$data <- smooth
  counts$centers[[1]] <- centers
  counts
}

