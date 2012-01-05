#' Standardise a summary to sum to one.
#' 
#' @param x
#' @param margin margins to standardise along.  If \code{NULL}, the default,
#'  standardises the whole array.
#' @export
#' @examples
#' b1 <- bin_nd(mtcars, "mpg", 0.01)
#' plot(b1)
#' d1 <- density_1d(b1, 0.5)
#' plot(d1)
#' # Note change in x-axis limits
#' plot(standardise(d1))
#'
#' # Can also standardise a dimension at a time
#' b2 <- bin_nd(mtcars, c("mpg", "wt"))
#' plot(b2)
#' plot(standardise(b2)) # doesn't look any different because no legend
#' plot(standardise(b2, 1)) # each row sums to 1
#' plot(standardise(b2, 2)) # each col sums to 1
standardise <- function(x, margin = NULL) {
  stopifnot(is.binned_summary(x))
  
  x$data <- prop.table(x$data, margin)
  x$data[is.na(x$data)] <- 0
  x
}
