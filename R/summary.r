#' Efficient binned 1d summaries.
#'
#' @param x a numeric vector to group by
#' @param y a numeric vector to summary for each group. Optional for some
#'   summary statistics.
#' @param summary the summary statistic to use. Currently must be one of
#'   count, sum, mean, median or sd. If \code{NULL}, defaults to mean if
#'   y is present, count if not.
#' @param weight a vector of weights.  Not currently supported by all summary
#'   functions.
#' @param binwidth,origin,breaks Binning specification. Either supply binwidth
#'  and optionally origin (defaults to \code{min(x)}), or a vector of breaks.
#' @export
summary1d <- function(x, y = NULL, summary = NULL, weights = NULL,
                      binwidth = NULL, origin = NULL, breaks = NULL) {
  if (is.null(summary)) {
    summary <- if (is.null(y)) "count" else "mean"
    message("Summarising with ", summary)
  }

  if (is.null(binwidth) + is.null(breaks) != 1L) {
    stop("You must specify one of binwidth and breaks", call. = FALSE)
  }

  if (!is.null(y)) {
    stopifnot(length(y) == length(x))
  } else {
    y <- 1
  }
  if (!is.null(weights)) {
    stopifnot(length(y) == length(x))
  } else {
    weights <- 1
  }

  if (!is.null(breaks)) {
    f <- match.fun(paste("compute", summary, "breaks", sep = "_"))
    f(x, y, weights, breaks = breaks)
  } else {
    if (is.null(origin)) {
      rng <- frange(x, na_rm = TRUE)
      origin <- rng[1]
    }
    f <- match.fun(paste("compute", summary, "fixed", sep = "_"))
    f(x, y, weights, width = binwidth, origin = origin)
  }

}
