#' Compute breaks given origin and width.
#'
#' Breaks are right-open, left-closed [x, y), so if \code{max(x)} is an integer
#' multiple of binwidth, then we need one more break. This function only returns
#' the left-side of the breaks.
#'
#' The first break is special, because it always contains missing values.
#'
#' @param x numeric vector
#' @param origin bin origin
#' @param binwidth bin width
#' @export
#' @keywords internal
#' @examples
#' breaks(10, origin = 0, binwidth = 1)
#' breaks(9.9, origin = 0, binwidth = 1)
#'
#' breaks(1:10, origin = 0, binwidth = 2)
breaks <- function(x, binwidth, origin = min(x)) {
  if (!is.binned(x)) {
    x <- bin(x, binwidth, origin)
  }

  # -1 for NA bin, -1 since R is 1 indexed
  nbins <- x$nbins() - 2
  c(NA, x$origin() + seq.int(1, nbins) * x$width())
}
