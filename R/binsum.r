#' Create/test for binned summary objects.
#'
#' @section Methods:
#'
#' Mathematical functions with methods for \code{binsum} object will modify
#' the x column of the data frame and \code{\link{rebin}} the data, calculating
#' updated summary statistics.
#'
#' Currently methods are provided for the \code{Math} group generic,
#' logical comparison and arithmetic operators, and
#' \code{\link[plyr]{round_any}}.
#'
#' @keywords internal
binsum <- function(df, type) {
  stopifnot(is.data.frame(df), ncol(df) >= 2)

  structure(df, class = c("binsum", class(df)), type = type)
}

#' @rdname binsum
#' @export
is.binsum <- function(x) {
  inherits(x, "binsum")
}

#' @S3method Math binsum
Math.binsum <- function(x, ...) {
  generic <- match.fun(.Generic)
  x[[1]] <- generic(x[[1]], ...)
  rebin(x)
}

logical_ops <- c("==", "!=", "<", "<=", ">=", ">")
math_ops <- c("+", "-", "*", "/", "^", "%%", "%/%")

#' @S3method Ops binsum
Ops.binsum <- function(e1, e2) {
  generic <- match.fun(.Generic)
  if (.Generic %in% logical_ops)  {
    generic(e1[[1]], e2)
  } else if (.Generic %in% math_ops) {
    e1[[1]] <- generic(e1[[1]], e2)
    rebin(e1)
  } else {
    stop(.Generic, " not supported for binsum objects", call. = FALSE)
  }
}

#' @S3method round_any binsum
round_any.binsum <- function(x, accuracy, f = round) {
  x[[1]] <- round_any(x[[1]], accuracy, f = f)
  rebin(x)
}
