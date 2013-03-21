#' dgrid: an S3 class for data grids
#'
#' @param x a numeric vector to test or coerce.
#' @param width bin width
#' @param origin bin origins
#' @param nbins number of bins
#' @export
#' @examples
#' g <- dgrid(0:10 + 0.5, width = 1)
#' range(g)
#' as.integer(g)
dgrid <- function(x, width, origin = 0, nbins = NULL) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(width), length(width) == 1, width > 0)
  stopifnot(is.numeric(origin), length(origin) == 1)

  if (is.null(nbins)) {
    nbins <- floor((max(x) - origin) / width)
  }

  structure(x, class = c("dgrid", "numeric"),
    width = width, origin = origin, nbins = nbins)
}

#' @export
#' @rdname dgrid
is.dgrid <- function(x) inherits(x, "dgrid")

#' @S3method [ dgrid
"[.dgrid" <- function(x, ...) {
  dgrid(NextMethod(), width = attr(x, "width"),
    origin = attr(x, "origin"), nbins = attr(x, "nbins"))
}

#' @S3method min dgrid
min.dgrid <- function(x, ...) attr(x, "origin")
#' @S3method max dgrid
max.dgrid <- function(x, ...) {
  min(x) + attr(x, "nbins") * attr(x, "width")
}
#' @S3method range dgrid
range.dgrid <- function(x, ...) c(min(x), max(x))

#' @S3method as.integer dgrid
as.integer.dgrid <- function(x, ...) {
  as.integer((unclass(x) - attr(x, "origin")) / attr(x, "width") + 1L)
}

#' @S3method as.data.frame dgrid
as.data.frame.dgrid <- function(x, ...) {
  n <- length(x)
  list <- list(x)
  class(list) <- "data.frame"
  attr(list, "row.names") <- c(NA_integer_, -n)
  list
}
