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
breaks <- function(x, origin, binwidth) {
  dest <- floor_any(max(x, na.rm = TRUE), binwidth)
  c(NA, seq(origin, dest, by = binwidth))
}
