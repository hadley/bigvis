#' Efficient binned summaries.
#'
#' @param ... group objects created by \code{\link{bin}}
#' @param z a numeric vector to summary for each group. Optional for some
#'   summary statistics.
#' @param summary the summary statistic to use. Currently must be one of
#'   count, sum, mean, median or sd. If \code{NULL}, defaults to mean if
#'   y is present, count if not.
#' @param w a vector of weights. Not currently supported by all summary
#'   functions.
#' @param drop if \code{TRUE} only locations with data will be returned.  This
#'   is more efficient if the data is very sparse (<1\% of cells filled), and
#'   is slightly less efficient. Defaults to \code{TRUE} if you are condensing
#'   over two or more dimensions, \code{FALSE} for 1d.
#' @export
#' @examples
#' x <- runif(1e5)
#' gx <- bin(x, 0.1)
#' condense(gx)
condense <- function(..., z = NULL, summary = NULL, w = NULL, drop = NULL) {
  gs <- list(...)
  if (length(gs) == 1 && is.list(gs[[1]])) gs <- gs[[1]]

  is_binned <- vapply(gs, is.binned, logical(1))
  if (!all(is_binned)) {
    stop("All objects passed to ... must be binned.", call. = FALSE)
  }

  drop <- drop %||% (length(gs) > 1)

  if (is.null(summary)) {
    summary <- if (is.null(z)) "count" else "mean"
    message("Summarising with ", summary)
  }

  # C++ code can deal with NULL inputs more efficiently than R code
  z <- z %||% numeric()
  w <- w %||% numeric()

  # Check lengths consistent
  n <- gs[[1]]$size()
  stopifnot(length(z) == 0 || length(z) == n)
  stopifnot(length(w) == 0 || length(w) == n)

  f <- match.fun(paste("condense", summary, sep = "_"))
  out <- f(gs, z, w, drop = drop)

  condensed(gs, out[[1]], out[[2]])
}
