#' Smooth 1d summary statistics
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
#' @param summary summary statistics produced by \code{\link{summary1d}}
#' @param bw smoothing bandwidth/standard deviation
#' @param var variable to smooth
#' @param grid,n A grid of locations to produced smoothed values. If \code{NULL},
#'   the default, will be an evenly spaced grid of \code{n} points across the
#'   the range of \code{x}.
#' @examples
#' x <- runif(1e5)
#' xsum <- summarise1d(x, binwidth = 1/100)
#' xsmu <- smooth1d(xsum, 1/100)
#'
#' plot(xsum)
#' lines(xsmu)
smooth1d <- function(summary, bw, var = names(summary)[2], grid = NULL, n = nrow(summary), reflect = TRUE) {
  x_rng <- frange(summary$x)

  if (is.null(grid)) {
    if (reflect) {
      g_rng <- x_rng
    } else {
      k_rng <- range(kernel)
      g_rng <- x_rng + c(-1, 1) / 2 * diff(k_rng)
    }
    message("Generating evenly spaced grid from ", format(g_rng[1]), " to ",
      format(g_rng[2]), " with ", n, " points")
    grid <- seq(g_rng[1], g_rng[2], length = n)
  }

  no_na <- summary[-1, ]
  s <- smooth_1d_normal(x = no_na$x[-1], z = no_na[[var]], x_out = grid, sd = bw)
  data.frame(x = c(NA, grid), s = c(summary[[var]][1], s))
}
