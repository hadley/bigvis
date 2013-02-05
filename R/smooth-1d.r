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
#' @param vars variables to smooth.  Defaults (with a message) to all variables
#'   in the original summary.
#' @param bw smoothing bandwidth/standard deviation. Defaults (with a message)
#'   to the minimum difference between bins. Since most of the normal density
#'   is located within 3 sd, this will smooth over the nearest two bins, with
#'   a small contribution from the nearest 3rd and 4th.
#' @param grid,n A grid of locations to produced smoothed values. If \code{NULL},
#'   the default, will be an evenly spaced grid of \code{n} points across the
#'   the range of \code{x}.
#' @examples
#' x <- runif(1e5)
#' xsum <- summarise1d(x, binwidth = 1/100)
#' xsmu <- smooth1d(xsum, bw = 1/100, n = 1000)
#'
#' plot(xsum)
#' lines(xsmu)
#'
#' # If you just want to distribute density, use standardise = FALSE
#' xsmu2 <- smooth1d(xsum, bw = 1/100, standardise = FALSE)
#' # notice the y-axis
#' plot(xsmu2, type = "l")
smooth1d <- function(summary, vars = NULL, bw = NULL, grid = NULL,
                     n = nrow(summary) - 1, reflect = TRUE, standardise = TRUE) {
  stopifnot(is.binsum(summary))

  x_rng <- frange(summary$x)
  no_na <- summary[-1, ]

  if (is.null(vars)) {
    vars <- names(summary)[-1]
    message("Smoothing ", paste(vars, collapse = ", "))
  }

  if (is.null(bw)) {
    bw <- min(diff(no_na$x))
    message("Bandwidth set to ", format(bw))
  }

  if (is.null(grid)) {
    if (reflect) {
      g_rng <- x_rng
    } else {
      k_rng <- range(kernel)
      g_rng <- x_rng + c(-1, 1) / 2 * diff(k_rng)
    }
    grid <- seq(g_rng[1], g_rng[2], length = n)
    if (!identical(all.equal(grid, no_na$x), TRUE)) {
      message("Generating evenly spaced grid from ", format(g_rng[1]), " to ",
        format(g_rng[2]), " with ", n, " points")
    }
  }

  smooth_var <- function(var) {
    s <- smooth_1d_normal(x = no_na$x, z = no_na[[var]], x_out = grid, sd = bw,
      standardise = standardise)
    c(summary[[var]][1], s)
  }
  smooths <- vapply(vars, smooth_var, numeric(nrow(summary)))
  binsum(data.frame(x = c(NA, grid), smooths), attr(summary, "type"))
}
