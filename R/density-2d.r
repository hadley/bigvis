#' 2d density estimation.
#'
#' The methods used in this function are adapted from the binned kernel density
#' estimation functions as used in MP Wand's \pkg{KernSmooth}.
#'
#' @param counts output from \code{\link{bin_2d}}
#' @param bandwidth smoothing parameter, a vector of length two. Should be
#'   relatively large compared to binwidth, otherwise data will not be smoothed.
#'
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
  if (l_x > 1) {
    grid_x <- seq(-4, 4, length = 2L * l_x + 1L)
    kernel_x <- matrix(dnorm(grid_x, sd = bandwidth[1]), ncol = 1)
  } else {
    warning("Bandwidth in x direction too small: no smoothing", call. = FALSE)
  }

  l_y <- trunc(bandwidth[2] / binwidth[2]) * 4L
  if (l_y > 1) {
    grid_y <- seq(-4, 4, length = 2L * l_y + 1L)
    kernel_y <- matrix(dnorm(grid_y, sd = bandwidth[2]), nrow = 1)
  } else {
    warning("Bandwidth in y direction too small: no smoothing", call. = FALSE)
  }

  smooth <- counts$data
  if (l_x > 1) smooth <- convolve_2d(smooth, kernel_x)
  if (l_y > 1) smooth <- convolve_2d(smooth, kernel_y)

  counts$data <- smooth
  counts$centers[[1]] <- expand_centers(counts$centers[[1]], l_x, binwidth[1])
  counts$centers[[2]] <- expand_centers(counts$centers[[2]], l_y, binwidth[2])
  counts
}
