#' A S3 class for caching the range of a vector
#'
#' For best performance, you may want to run copy and paste the contents of
#' this function into your function, to avoid making any copies of \code{x}.
#'
#' @param x a numeric vector
#' @param range the range of the vector (excluding missing values), if known.
#'   If unknown, it will be computed with \code{\link{frange}}, a fast C++
#'   implementation of \code{\link{range}}.
#' @export
#' @examples
#' x <- runif(1e6)
#' y <- ranged(x)
#' range(y)
#'
#' # Modifications to the class currently destroy the cache
#' y[1] <- 10
#' max(y)
#' class(y)
ranged <- function(x, range = frange(x, na_rm = TRUE)) {
  stopifnot(is.numeric(x))

  # Reset range attribute so that lazy evaluation of range
  # always recomputes from scratch
  attr(x, "range") <- NULL

  attr(x, "range") <- range
  class(x) <- "ranged"
  x
}

#' @S3method min ranged
min.ranged <- function(x, ...) attr(x, "range")[1]
#' @S3method max ranged
max.ranged <- function(x, ...) attr(x, "range")[2]
#' @S3method range ranged
range.ranged <- function(x, ...) attr(x, "range")

#' @S3method [<- ranged
"[<-.ranged" <- function(x, ...) {
  attr(x, "range") <- NULL
  attr(x, "class") <- NULL
  NextMethod(x, ...)
}

#' @S3method as.data.frame ranged
as.data.frame.ranged <- function(x, ...) {
  n <- length(x)
  x <- list(x)
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -n)

  x
}
