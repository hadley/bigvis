#' John-Draper transformation.
#'
#' A generalisation of the box-cox transformation that works for
#' values with both positive and negative values.
#'
#' @param x values to transform
#' @param lambda degree of transformation:
#' @export
#' @examples
#' x <- seq(-10, 10, length = 100)
#' plot(x, jd(x, 0), type = "l")
#' plot(x, jd(x, 1), type = "l")
#' plot(x, jd(x, 2), type = "l")
#' plot(x, jd(x, -1), type = "l")
#' plot(x, jd(x, -2), type = "l")
jd <- function(x, lambda) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(lambda), length(lambda) == 1)

  if (lambda == 0) {
    sign(x) * log(abs(x) + 1)
  } else {
    sign(x) * ((abs(x) + 1) ^ lambda - 1) / lambda
   }
}
