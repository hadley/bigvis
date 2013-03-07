#' Create a binned variable.
#'
#' @details
#' This function produces an R reference class that wraps around a C++ function.
#' Generally, you should just treat this as an opaque object with reference
#' semantics, and you shouldn't call the methods on it - pass it to
#' \code{\link{condense}} and friends.
#'
#' @param x numeric or integer vector
#' @param width bin width
#' @param origin if not specified, guessed by \code{\link{find_origin}}
#' @param name name of original variable. This will be guess from the input to
#'   \code{group} if not supplied. Used in the output of
#'   \code{\link{condense}} etc.
#' @export
#' @examples
#' x <- runif(1e6)
#' g <- bin(x, 0.01)
#' g
bin <- function(x, width, origin = NULL, name = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (!is.ranged(x)) {
    attr(x, "range") <- frange(x)
    class(x) <- "ranged"
  }
  origin <- origin %||% find_origin(x, width)

  BigVis$BinnedVector$new(x, name, width, origin)
}

setMethod("show", "Rcpp_BinnedVector", function(object) {
  cat("Binned [", object$size(), "]. ",
    "Width: ", object$width(), " Origin: ", object$origin(), "\n", sep = "")
})

is.binned <- function(x) {
  is(x, "Rcpp_BinnedVector")
}

bins <- function(...) {
  BigVis$BinnedVectors$new(list(...))
}

