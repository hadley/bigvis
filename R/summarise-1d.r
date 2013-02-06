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
#' @examples
#' x <- runif(1e5)
#' summarise1d(x, binwidth = 0.1)
summarise1d <- function(x, z = NULL, summary = NULL, w = NULL,
                      binwidth = NULL, origin = NULL, breaks = NULL) {
  if (is.null(summary)) {
    summary <- if (is.null(z)) "count" else "mean"
    message("Summarising with ", summary)
  }
  stopifnot(summary %in% names(summaries))

  if (is.null(binwidth) + is.null(breaks) != 1L) {
    stop("You must specify one of binwidth and breaks", call. = FALSE)
  }

  # C++ code can deal with NULL inputs more efficiently than R code
  z <- z %||% numeric()
  w <- w %||% numeric()

  # Check lenghts consistent
  stopifnot(length(z) == 0 || length(z) == length(x))
  stopifnot(length(w) == 0 || length(w) == length(x))

  if (!is.null(breaks)) {
    f <- match.fun(paste("compute", summary, "breaks", sep = "_"))
    out <- f(x, z, weights, breaks = breaks)
  } else {
    origin <- origin %||% find_origin(x, binwidth)

    f <- match.fun(paste("summarise", summary, "fixed", sep = "_"))
    out <- f(x, z, w, width = binwidth, origin = origin)
    breaks <- origin + binwidth * seq_len(nrow(out) - 1)
  }

  binsum(data.frame(x = c(NA, breaks), out), summary_class[[summary]])
}
