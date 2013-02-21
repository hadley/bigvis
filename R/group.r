module <- Module("Group")

#' Create a grouped variable.
#'
#' @details
#' This function produces an R reference class that wraps around a C++ function.
#' Generally, you should just treat this as an opaque object with reference
#' semantics, and you shouldn't call the methods on it - pass it to
#' \code{\link{summarise_1d}} and friends.
#'
#' @param x numeric or integer vector
#' @param width bin width
#' @param origin if not specified, guessed by \code{\link{find_origin}}
#' @export
#' @examples
#' x <- runif(1e6)
#' g <- grouped(x, 0.01)
grouped <- function(x, width, origin = NULL) {
  if (!is.ranged(x)) x <- ranged(x)
  origin <- origin %||% find_origin(x, width)

  module$GroupFixed$new(x, width, origin)
}

setMethod("show", "Rcpp_GroupFixed", function(object) {
  cat("Grouped [", object$size(), "]. ",
    "Width: ", object$width(), " Origin: ", object$origin(), "\n", sep = "")
})
