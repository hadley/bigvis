smooth_1d <- function(summary, var = names(summary)[2], kernel = kernel("norm", sd = 1),
  grid = NULL, n = nrow(summary), reflect = TRUE) {
  stopifnot(is.kernel(kernel))

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

  convolve_1d(summary$x, summary[var], grid, kernel, reflect = reflect)

  # Generate the kernel
  l <- trunc(bandwidth / binwidth) * 4L
  if (l < 1) {
    warning("Bandwidth too small: no smoothing", call. = FALSE)
    return(summary)
  }

  grid <- seq(-4, 4, length = 2L * l + 1L)
  kernel <- dnorm(grid, sd = bandwidth)

  # Smooth, by convolving data with kernel
  smooth <- array(convolve_1d(counts$data, kernel))

  counts$data <- smooth
  counts$centers[[1]] <- expand_centers(counts$centers[[1]], l, binwidth)
  counts
}

#' A trimmed kernel function.
#'
#' The resulting kernel will not integrate to 1: the assumption is that the
#' mass outside the range is negligible. This is done for efficiency reasons.
kernel <- function(dist, ..., range = c(0.005, 0.995)) {
  stopifnot(is.character(dist), length(dist) == 1)
  qdist <- match.fun(paste("q", dist, sep = ""))
  pdist <- match.fun(paste("p", dist, sep = ""))

  krange <- qdist(range, ...)

  structure(function(x) pdist(x, ...), class = "kernel")
}

is.kernel <- function(x) inherits(x, "kernel")
range.kernel <- function(x, ...) environment(x)$krange


