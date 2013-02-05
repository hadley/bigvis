#' Create a new binned summary object.
#'
#' @keywords inherit.
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
