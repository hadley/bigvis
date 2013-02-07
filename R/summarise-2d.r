summarise2d <- function(x, y, z = NULL, summary = NULL, w = NULL,
                        x_binwidth, x_origin = NULL,
                        y_binwidth, y_origin = NULL) {
  if (is.null(summary)) {
    summary <- if (is.null(z)) "count" else "mean"
    message("Summarising with ", summary)
  }
  stopifnot(summary %in% names(summaries))

  # Compute ranges only once
  if (!is.ranged(x)) x <- ranged(x)
  if (!is.ranged(y)) y <- ranged(y)

  # C++ code can deal with NULL inputs more efficiently than R code
  z <- z %||% numeric()
  w <- w %||% numeric()

  # Check lengths consistent
  stopifnot(length(x) == length(y))
  stopifnot(length(z) == 0 || length(z) == length(x))
  stopifnot(length(w) == 0 || length(w) == length(x))

  x_origin <- x_origin %||% find_origin(x, x_binwidth)
  y_origin <- y_origin %||% find_origin(y, y_binwidth)

  f <- match.fun(paste("summarise", summary, "2dfixed", sep = "_"))
  out <- f(x, y, z, w, x_width = x_binwidth, y_width = y_binwidth,
    x_origin = x_origin, y_origin = y_origin)

  x_breaks <- breaks(x, x_origin, x_binwidth)
  y_breaks <- breaks(y, y_origin, y_binwidth)
  breaks <- expand.grid(x = x_breaks, y = y_breaks, KEEP.OUT.ATTRS = FALSE)

  binsum(cbind(breaks, out), summary_class[[summary]])
}

