#' Standardise a summary to sum to one.
#'
#' @param x
#' @param margin margins to standardise along.  If \code{NULL}, the default,
#'  standardises the whole array.
#' @export
#' @examples
#' b1 <- condense(bin(movies$year, 1))[-1, ]
#' d1 <- smooth(b1, 2, type = "reg")
#'
#' if (require("ggplot2")) {
#'
#' autoplot(b1)
#' autoplot(d1)
#'
#' # Note change in x-axis limits
#' autoplot(standardise(d1))
#' }
#'
#' # Can also standardise a dimension at a time
#' b2 <- with(movies, condense(bin(year, 1), bin(length, 20)))
#'
#' if (require("ggplot2")) {
#'
#' base <- ggplot(b2, aes(length, .count)) +
#'   geom_line(aes(group = year))
#' base
#' base %+% standardise(b2)
#' base %+% standardise(b2, "length")
#' base %+% standardise(b2, "year")
#'
#' autoplot(b2)
#' autoplot(standardise(b2))    # note legend
#' autoplot(standardise(b2, "length")) # each row sums to 1
#' autoplot(standardise(b2, "year")) # each col sums to 1
#' }
standardise <- function(x, margin = integer()) {
  stopifnot(is.condensed(x), !is.null(x$.count))

  if (length(margin) == 0) {
    x$.count <- prop.table(x$.count)
  } else {
    x$.count <- ave(x$.count, id(x[margin]), FUN = prop.table)
    x$.count[is.na(x$.count)] <- 0
  }

  x
}
