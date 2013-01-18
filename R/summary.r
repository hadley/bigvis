
summary1d <- function(x, y = NULL, summary = NULL, weights = NULL,
                      binwidth = NULL, origin = NULL, breaks = NULL) {
  if (is.null(summary)) {
    summary <- if (is.null(y)) "count" else "mean"
    message("Summarising with ", summary)
  }

  if (is.null(binwidth) + is.null(breaks) != 1L) {
    stop("You must specify one of binwidth and breaks", call. = FALSE)
  }

  if (!is.null(y)) stopifnot(length(y) == length(x))
  if (!is.null(weights)) stopifnot(length(y) == length(x))

  if (!is.null(breaks)) {
    f <- match.fun(paste("compute", summary, "breaks", sep = "_"))
    f(x, y, weights, breaks = breaks)
  } else {
    f <- match.fun(paste("compute", summary, "fixed", sep = "_"))
    f(x, y, weights, binwidth = binwidth, origin = origin)
  }

}
