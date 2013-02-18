#' Compute and sum derivatives of the normal distribution.
#'
#' Support function for Sheather-Johns plugin estimate of the bandwidth.
#'
#' @param x locations at which to evaluate derivatives
#' @param w optional weights
#' @param order order of derivation, currently up to order 6.
#' @keywords internal
#' @export
norm_deriv <- function(x, w, order = 1L) {
  stopifnot(is.integer(order), length(order) == 1, order > 0)
  if (order > length(hermite)) {
    stop("Only know hermite polynomials up to order" , length(hermite))
  }
  fx <- exp(-x^2 / 2) * hermite[[order]](x) / sqrt(2 * pi)
  sum(fx * w)
}

# The first six probabilists Hermite polynomials
hermite <- list(
  function(x) x,
  function(x) x ^ 2 -  1,
  function(x) x ^ 3 -  3 * x,
  function(x) x ^ 4 -  6 * x ^ 2 + 3,
  function(x) x ^ 5 - 10 * x ^ 3 + 15 * x,
  function(x) x ^ 6 - 15 * x ^ 4 + 45 * x ^ 2 - 15
)


