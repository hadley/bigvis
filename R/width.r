#' Compute a reasonable default binwidth.
#'
#' @param x a numeric vector. If a numeric vector of length one is supplied,
#'   it's assumed that
#' @param nbins desired number of bins (approximate)
#' @export
#' @keywords internal
#' @family reasonable defaults
#' @examples
#' find_width(c(0, 5))
#' find_width(c(0, 5.023432))
#' find_width(c(0, 5.9))
find_width <- function(x, nbins = 1e4) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(nbins), length(nbins) == 1, nbins > 0)

  x <- diff(frange(x))
  size <- x / nbins

  # divide into order of magnitude and multiplier
  om <- 10 ^ ceiling(log10(size))
  mult <- size / om

  # ensure number per unit is multiple of 1, 2, 3, 4, or 5
  per_unit <- 1 / mult
  rounders <- c(1, 2, 3, 4, 5)
  poss <- round(per_unit / rounders) * rounders
  poss <- poss[poss != 0]
  width <- om / poss[which.min(abs(poss - per_unit))]

  structure(width, n = ceiling(x / width), per_unit = 1 / width)
}
