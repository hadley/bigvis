#' Find the origin.
#'
#' @details
#' This algorithm implements simple heuristics for determining the origin of
#' a histogram when only the binwidth is specified. It:
#'
#' \itemize{
#'    \item rounds to zero, if relatively close
#'    \item subtracts 0.5 offset, if an x is integer
#'    \item ensures the origin is a multiple of the binwidth
#' }
#' @param x numeric or integer vector
#' @param binwidth binwidth
#' @export
#' @keywords internal
#' @examples
#' find_origin(1:10, 1)
#' find_origin(1:10, 2)
#' find_origin(c(1, 1e6), 1)
find_origin <- function(x, binwidth) {
  rng <- frange(x, finite = TRUE)
  if (!all(is.finite(rng))) stop("No valid values in x", call. = FALSE)

  offset <- is.integer(x) * 0.5

  if (close_to_zero(rng[1], rng)) {
    0 - offset
  } else {
    floor_any(rng[1], binwidth) - offset
  }
}

close_to_zero <- function(x, rng) {
  (abs(x) / abs(rng[2] - rng[1])) < 1e-3
}

floor_any <- function(x, accuracy) {
  floor(x / accuracy) * accuracy
}
