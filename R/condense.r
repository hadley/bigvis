#' Efficient binned summaries.
#'
#' @param x a group object created by \code{\link{grouped}}
#' @param y a numeric vector to summary for each group. Optional for some
#'   summary statistics.
#' @param summary the summary statistic to use. Currently must be one of
#'   count, sum, mean, median or sd. If \code{NULL}, defaults to mean if
#'   y is present, count if not.
#' @param weight a vector of weights.  Not currently supported by all summary
#'   functions.
#' @param drop if \code{TRUE} only locations with data will be returned.  This
#'   is more efficient if the data is very sparse (<1\% of cells filled), and
#'   is slightly less efficient. Defaults to \code{TRUE} if you are condensing
#'   over two or more dimensions, \code{FALSE} for 1d.
#' @export
#' @examples
#' x <- runif(1e5)
#' gx <- grouped(x, 0.1)
#' condense(gx)
condense <- function(x, z = NULL, summary = NULL, w = NULL, drop = NULL) {
  if (is.grouped(x)) {
    x <- list(x)
  } else if (is.list(x)) {

  } else {
    stop("x must be a list or a single grouped object", call. = FALSE)
  }

  drop <- drop %||% (length(x) > 1)

  if (is.null(summary)) {
    summary <- if (is.null(z)) "count" else "mean"
    message("Summarising with ", summary)
  }
  stopifnot(summary %in% names(summaries))

  # C++ code can deal with NULL inputs more efficiently than R code
  z <- z %||% numeric()
  w <- w %||% numeric()

  # Check lengths consistent
  n <- x[[1]]$size()
  stopifnot(length(z) == 0 || length(z) == n)
  stopifnot(length(w) == 0 || length(w) == n)

  f <- match.fun(paste("condense", summary, sep = "_"))
  out <- f(x, z, w, drop = drop)

  data.frame(out[[2]], out[[1]])
}
