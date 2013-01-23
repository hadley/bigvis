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
summarise1d <- function(x, z = NULL, summary = NULL, weights = NULL,
                      binwidth = NULL, origin = NULL, breaks = NULL) {
  if (is.null(summary)) {
    summary <- if (is.null(z)) "count" else "mean"
    message("Summarising with ", summary)
  }
  stopifnot(summary %in% names(summaries))

  if (is.null(binwidth) + is.null(breaks) != 1L) {
    stop("You must specify one of binwidth and breaks", call. = FALSE)
  }

  if (!is.null(z)) {
    stopifnot(length(z) == length(x))
  } else {
    y <- 1
  }
  if (!is.null(weights)) {
    stopifnot(length(weights) == length(x))
  } else {
    weights <- 1
  }

  if (!is.null(breaks)) {
    f <- match.fun(paste("compute", summary, "breaks", sep = "_"))
    out <- f(x, y, weights, breaks = breaks)
  } else {
    if (is.null(origin)) {
      rng <- frange(x, na_rm = TRUE)
      origin <- rng[1]
      if (abs(origin) / abs(rng[2] - rng[1]) < 1e-3) origin <- 0
    }
    f <- match.fun(paste("summarise", summary, "fixed", sep = "_"))
    out <- f(x, z, weights, width = binwidth, origin = origin)
    breaks <- origin + binwidth * seq_len(nrow(out) - 1)
  }

  data.frame(x = c(NA, breaks), z = out)
}

groups <- list(
  Breaks = c(breaks = "NumericVector&"),
  Fixed = c(width = "double", origin = "double")
)
summaries <- c(
  count = "Sum(0)",
  sum = "Sum(1)",
  mean = "Moments(1)",
  sd = "Moments(2)",
  median = "Median()"
)


