
#' Create a binned variable.
#'
#' @details
#' This function produces an R reference class that wraps around a C++ function.
#' Generally, you should just treat this as an opaque object with reference
#' semantics, and you shouldn't call the methods on it - pass it to
#' \code{\link{condense}} and friends.
#'
#' @param x numeric or integer vector
#' @param width bin width. If not specified, about 10,000 bins will be chosen
#'   using the algorithim in \code{\link{find_width}}.
#' @param origin origin. If not specified, guessed by \code{\link{find_origin}}.
#' @param name name of original variable. This will be guessed from the input to
#'   \code{group} if not supplied. Used in the output of
#'   \code{\link{condense}} etc.
#' @export
#' @examples
#' x <- runif(1e6)
#' bin(x)
#' bin(x, 0.01)
#' bin(x, 0.01, origin = 0.5)
bin <- function(x, width = find_width(x), origin = find_origin(x, width),
                name = NULL) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(width), length(width) == 1, width > 0)
  stopifnot(is.numeric(origin), length(origin) == 1)

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }
  stopifnot(is.character(name), length(name) == 1)

  if (!is.ranged(x)) {
    attr(x, "range") <- frange(x)
    class(x) <- "ranged"
  }
  if (origin > min(x)) {
    warning("Origin larger than min(x): some values will be truncated",
      call. = FALSE)
  }

  BigVis$BinnedVector$new(x, name, width, origin)
}


is.binned <- function(x) {
  is(x, "Rcpp_BinnedVector")
}

bins <- function(...) {
  BigVis$BinnedVectors$new(list(...))
}

