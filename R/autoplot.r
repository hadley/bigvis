#' Autoplot condensed summaries.
#'
#' @param x a condensed summary
#' @param var which summary variable to display
#' @param ... other arguments passed on to individual methods
#' @method autoplot condensed
#' @export autoplot.condensed
#' @examples
#' if (require("ggplot2")) {
#'
#' # 1d summaries -----------------------------
#' x <- rchallenge(1e4)
#' z <- x + rt(length(x), df = 2)
#' xsum <- condense(bin(x, 0.1))
#' zsum <- condense(bin(x, 0.1), z = z)
#'
#' autoplot(xsum)
#' autoplot(peel(xsum))
#'
#' autoplot(zsum)
#' autoplot(peel(zsum, keep = 1))
#' autoplot(peel(zsum))
#'
#' # 2d summaries -----------------------------
#' y <- runif(length(x))
#' xysum <- condense(bin(x, 0.1), bin(y, 0.1))
#' xyzsum <- condense(bin(x, 0.1), bin(y, 0.1), z = z)
#'
#' autoplot(xysum)
#' autoplot(peel(xysum))
#' autoplot(xyzsum)
#' autoplot(peel(xyzsum))
#' }
autoplot.condensed <- function(x, var = last(summary_vars(x)), ...) {
  stopifnot(is.condensed(x))
  stopifnot(is.character(var), length(var) == 1)
  summaries <- c(
    .count = "total",
    .sum = "total",
    .mean = "summary",
    .sd = "summary",
    .median = "summary"
  )
  if (!(var %in% names(summaries))) {
    stop("Unknown varible", call. = FALSE)
  }
  d <- gcol(x)
  if (d > 2) {
    stop("No autoplot methods available for more than two d")
  }

  f <- paste0("plot_", summaries[var], "_", d)
  match.fun(f)(x, var = var, ...)
}


plot_total_1 <- function(x, var = ".count", show_na = TRUE, log = "") {
  xvar <- names(x)[[1]]

  plot <- ggplot(x[-1, ], aes_string(x = xvar, y = var)) +
    geom_line(na.rm = TRUE)

  if (show_na) {
    plot <- plot + na_layer(x, var)
  }

  if (logv(log, "y")) {
    plot <- plot + scale_y_continuous(trans = "log1p")
  }
  if (logv(log, "x")) {
    plot <- plot + scale_x_log10()
  }

  plot
}

plot_total_2 <- function(x, var = ".count", show_na = TRUE, log = "") {
  x <- peel(x, keep = 1)
  xvar <- names(x)[[1]]
  yvar <- names(x)[[2]]
  miss <- is.na(x[[1]]) + 2 * is.na(x[[2]])

  fill_trans <- if (logv(log, "z")) "log1p" else "identity"

  plot <- ggplot(x[miss == 0, ], aes_string(x = xvar, y = yvar)) +
    geom_raster(aes_string(fill = var)) +
    scale_fill_gradient(low = "grey90", high = "black", trans = fill_trans) +
    expand_limits(fill = 0)

  if (show_na) {
  }

  plot <- plot + if (logv(log, "x")) scale_x_log10()
  plot <- plot + if (logv(log, "y")) scale_y_log10()

  plot
}

plot_summary_1 <- function(x, var = ".mean", show_na = TRUE,
                                    show_n = x %contains% ".count", log = NULL) {
  xvar <- names(x)[[1]]

  plot <- ggplot(x[-1, ], aes_string(x = xvar, y = var)) +
    geom_line(na.rm = TRUE) +
    scale_size_area()

  if (show_n) {
    plot <- plot +
      geom_point(aes(color = .count), na.rm = TRUE) +
      scale_colour_gradient(trans = "log10")
  }

  if (show_na) {
    plot <- plot + na_layer(x, var)
  }

  plot
}

plot_summary_2 <- function(x, var = ".mean", show_na = TRUE, log = "") {
  x <- peel(x, keep = 1)
  xvar <- names(x)[[1]]
  yvar <- names(x)[[2]]

  miss <- is.na(x[[1]]) + 2 * is.na(x[[2]])

  plot <- ggplot(x[miss == 0, ], aes_string(x = xvar, y = yvar)) +
    geom_tile(aes_string(fill = var)) +
    scale_fill_gradient2()

  if (show_na) {
  }

  plot <- plot + if (logv(log, "x")) scale_x_log10()
  plot <- plot + if (logv(log, "y")) scale_y_log10()

  plot
}

na_layer <- function(x, var) {
  val <- x[[var]][is.na(x[[1]])]
  if (length(val) == 0 || is.na(val) || val == 0) return()

  xloc <- miss_poss(x[[1]])
  annotate("text", x = xloc, y = val, colour = "red", label = "NA",
    size = 3)
}

logv <- function(log, var) var %in% strsplit(log, "")[[1]]

miss_poss <- function(x) {
  rng <- frange(x)
  rng[1] - (rng[2] - rng[1]) * 0.05
}
