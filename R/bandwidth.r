#
# Translated from bw.SJ and underlying C code
#
# Copyright (C) 2013      Hadley Wickham
# Copyright (C) 2012      The R Core Team
# Copyright (C) 1994-2001 W. N. Venables and B. D. Ripley
#


#' Guess the best bandwidth for smoothing a count summary.
#'
#' @details
#' This uses the Sheather-Jones estimator as described in the reference.
#' It was chosen because it is efficient to compute, appears to perform
#' well in simulation studies, and a reference implementation for non-binned
#' data already existed.
#'
#' @param binsum a \code{\link{binsum}} object.
#' @author This code was adapted from the R and C code underlying
#'  \code{\link[stats]{bw.SJ}}, written Bill Venable and Brian Ripley.
#' @export
#' @examples
#' x <- runif(1e3)
#' xsum <- summarise_1d(x, binwidth = 1/10)
#' guess_bandwidth(xsum)
guess_bandwidth <- function(binsum) {
  stopifnot(is.binsum(binsum))
  stopifnot(!is.null(binsum$count))
  # Don't use missing values when computing binwidth
  x <- binsum$x[-1]
  w <- binsum$count[-1]
  n <- sum(w)

  # This needs to be replaced by better metadata on the binsum object
  bw <- x[2] - x[1]

  mid_bin <- trunc(x / bw + 1/2)
  cnt <- double_diff_sum(mid_bin, w)
  xb <- seq.int(0, length(cnt) - 1) * bw

  scale <- weighted.sd(x, w)
  # scale <- min(weighted.sd(x), weighted.IQR(x, w) / 1.349)
  # 1.349 = ratio of IQR to sd for normal distribution

  a <- 1.24 * scale * n ^ (-1 / 7)
  b <- 1.23 * scale * n ^ (-1 / 9)
  # 1.23 = 0.920 * 1.349; 1.24 = 0.912 * 1.349
  # i.e. reparameterising from IQR in paper to sd in code. 0.912 and 0.920 are
  # given without derivation, but are apparently described/derived in
  # Jones and Sheather (1991) "Using non-stochastic terms to advantage in
  # kernel-based estimation of integrated squared density derivatives"

  # Compute important constants for final calculation
  SDh <- function(h)  norm_deriv(xb / h, cnt, 4L) / (n * (n - 1) * h ^ 5)
  TDh <- function(h) -norm_deriv(xb / h, cnt, 6L) / (n * (n - 1) * h ^ 7)

  # "roughness" of standard normal kernel
  RK <- 1 / (2 * sqrt(pi))
  # Compared to paper, h moved out of alpha2 into fSD
  alpha2 <- 1.357 * (SDh(a) / TDh(b)) ^ (1 / 7)
  if (!is.finite(alpha2)) {
    stop("Sample too sparse to find bandwidth", call. = FALSE)
  }

  # Numerically find root of fSD
  fSD <- function(h) (RK / (n * SDh(alpha2 * h ^ (5 / 7)))) ^ (1 / 5) - h

  # Guess and expand upper and lower limits for root finding
  upper <- 1.144 * scale * n ^ (-1 / 5)
  lower <- upper * 0.1
  limits <- expand_limits(fSD, lower, upper)
  tol <- lower / 10

  uniroot(fSD, limits, tol = tol)$root
}


# Progressively expand limits for root finding
expand_limits <- function(f, lower, upper, tries = 100) {
  try <- 1L
  while (f(lower) * f(upper) > 0) {
    if (try > 99L) {
      stop("Numerical optimisation failed", call. = FALSE)
    }

    if (try %% 2) {
      upper <- upper * 1.2
    } else {
      lower <- lower / 1.2
    }

    try <- try + 1L
  }
  c(lower, upper)
}
