#Binned kernel density estimation in two dimensions (using normal kernels).
#This function takes the output object from the "bin_2d" function. The other 
#variable, "bandwidth", is a smoothing parameter. note that for low numbers
#of bins, a higher smoothing parameter is needed. 
#The methods used in this function are adapted from the binned kernel density 
#estimation functions in MP Wand's "KernSmooth" package.

#' 2d density estimation.
#' 
#' @examples
#' b2 <- bin_nd(mtcars, c("mpg", "wt"), binwidth = 0.1)
#' plot(b2)
#' s2 <- density_2d(b2, c(.5, .5))
#' contour(s2, add = T)
#' plot(s2)
#'
#' time <- bin_nd(movies, c("year", "length"))
#' plot(time)
#' time <- time[, 1:150]
#' plot(time)
#' plot(density_2d(time, c(1, 1)))
density_2d <- function(counts, bandwidth) {
  stopifnot(is.binned_summary(counts)) # must come from bin_nd
  stopifnot(length(dim(counts)) == 2)  # must be 2d
  
  binwidth <- unlist(counts$binwidth)
  stopifnot(!is.na(binwidth))          # must be continuous

  stopifnot(length(bandwidth) == length(binwidth))
  
  # Generate the two separable kernels
  l_x <- trunc(bandwidth[1] / binwidth[1]) * 4L
  grid_x <- seq(-4, 4, length = 2L * l_x + 1L)
  kernel_x <- matrix(dnorm(grid_x, sd = bandwidth[1]), ncol = 1)

  l_y <- trunc(bandwidth[1] / binwidth[1]) * 4L
  grid_y <- seq(-4, 4, length = 2L * l_y + 1L)
  kernel_y <- matrix(dnorm(grid_y, sd = bandwidth[2]), nrow = 1)

  smooth <- counts$data
  smooth <- convolve_2d(smooth, kernel_x)
  smooth <- convolve_2d(smooth, kernel_y)
  
  counts$data <- smooth
  counts$centers[[1]] <- expand_centers(counts$centers[[1]], l_x, binwidth[1])
  counts$centers[[2]] <- expand_centers(counts$centers[[2]], l_y, binwidth[2])
  counts
}
