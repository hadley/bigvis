#' Modulus transformation (and its inverse).
#'
#' A generalisation of the box-cox transformation that works for
#' values with both positive and negative values.
#'
#' This is useful for compressing the tails of long-tailed distributions,
#' often encountered with very large datasets.
#'
#' @param x values to transform
#' @param lambda degree of transformation
#' @export
#' @references J. John and N. Draper. "An alternative family of
#'  transformations." Applied Statistics, pages 190-197, 1980.
#'  \url{http://www.jstor.org/stable/2986305}
#' @examples
#' x <- seq(-10, 10, length = 100)
#' plot(x, mt(x, 0), type = "l")
#' plot(x, mt(x, 0.25), type = "l")
#' plot(x, mt(x, 0.5), type = "l")
#' plot(x, mt(x, 1), type = "l")
#' plot(x, mt(x, 2), type = "l")
#' plot(x, mt(x, -1), type = "l")
#' plot(x, mt(x, -2), type = "l")
mt <- function(x, lambda) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(lambda), length(lambda) == 1)

  if (lambda == 0) {
    sign(x) * log(abs(x) + 1)
  } else {
    sign(x) * ((abs(x) + 1) ^ lambda - 1) / lambda
   }
}

#' @rdname mt
#' @export
inv_mt <- function(x, lambda) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(lambda), length(lambda) == 1)

  if (lambda == 0) {
    sign(x) * (exp(abs(x)) - 1)
  } else {
    sign(x) * ((abs(x) * lambda + 1) ^ (1 / lambda) - 1)
   }
}

#' @rdname mt
#' @export
mt_trans <- function(lambda) {
  stopifnot(require("scales"))

  trans_new("modulo",
    function(x) mt(x, lambda),
    function(x) inv_mt(x, lambda)
  )
}
