#' A S3 class for caching the range of a vector
#'
#' This class is designed for dealing with large vectors, where the cost of
#' recomputing the range multiple times is prohibitive. It provides methods
#' for \code{\link{print}} and \code{\link{str}} that display only the range,
#' not the contents.
#'
#' @section Performance:
#' For best performance, you may want to run copy and paste the contents of
#' this function into your function, to avoid making any copies of \code{x}.
#' This is probably only necessary if you're dealing with extremely large
#' vectors, > 100 million obs.
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
#' y
#' str(y)
#'
#' # Modifications to the class currently destroy the cache
#' y[1] <- 10
#' max(y)
#' class(y)
#' z <- y + 10
#' max(z)
#' class(z)
ranged <- function(x, range = frange(x, finite = TRUE)) {
  stopifnot(is.numeric(x))

  # Reset range attribute so that lazy evaluation of range
  # always recomputes from scratch
  attr(x, "range") <- NULL

  attr(x, "range") <- range
  class(x) <- "ranged"
  x
}

#' Test if an object is of class ranged.
#'
#' @export
#' @param x object to test
#' @keywords internal
is.ranged <- function(x) inherits(x, "ranged")

#' @S3method min ranged
min.ranged <- function(x, ...) attr(x, "range")[1]
#' @S3method max ranged
max.ranged <- function(x, ...) attr(x, "range")[2]
#' @S3method range ranged
range.ranged <- function(x, ...) attr(x, "range")

#' @S3method print ranged
print.ranged <- function(x, ...) {
  rng <- attr(x, "range")
  # attr(x, "range") <- NULL
  # attr(x, "class") <- NULL
  # print.default(x)
  cat("Ranged 1:", length(x), " [", format(rng[1]), ", ", format(rng[2]), "]\n",
    sep = "")
}

#' @S3method str ranged
str.ranged <- function(object, ...) {
  rng <- attr(object, "range")
  cat(" Ranged [1:", length(object), "] ", format(rng[1]), "--", format(rng[2]),
    "\n", sep = "")
}

#' @S3method Ops ranged
Ops.ranged <- function(e1, e2) {
  attr(e1, "range") <- NULL
  class(e1) <- NULL

  NextMethod(e1, e2)
}

#' @S3method [<- ranged
"[<-.ranged" <- function(x, ..., value) {
  attr(x, "range") <- NULL
  attr(x, "class") <- NULL
  NextMethod(x, ..., value = value)
}

#' @S3method as.data.frame ranged
as.data.frame.ranged <- function(x, ...) {
  n <- length(x)
  x <- list(x)
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -n)

  x
}
