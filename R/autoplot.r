autoplot.binsum_1d_sum <- function(x, var = "count", show_na = TRUE, log = "") {
  name <- deparse(substitute(x))

  plot <- ggplot(x[-1, ], aes_string(x = "x", y= var)) +
    geom_line(na.rm = TRUE) +
    xlab(name)

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

autoplot.binsum_2d_sum <- function(x, var = "count", show_na = TRUE, log = "") {
  x <- x[x$count > 0, ]
  miss <- is.na(x$x) + 2 * is.na(x$y)

  fill_trans <- if (logv(log, "z")) "log1p" else "identity"

  plot <- ggplot(x[miss == 0, ], aes_string(x = "x", y = "y")) +
    geom_raster(aes_string(fill = var)) +
    scale_fill_gradient(low = "grey90", high = "black", trans = fill_trans) +
    expand_limits(fill = 0)

  if (show_na) {
  }

  plot <- plot + if (logv(log, "x")) scale_x_log10()
  plot <- plot + if (logv(log, "y")) scale_y_log10()

  plot
}

autoplot.binsum_1d_moments <- function(x, var = "mean", show_na = TRUE,
                                    show_n = TRUE, log = NULL) {
  name <- deparse(substitute(x))

  plot <- ggplot(x[-1, ], aes_string(x = "x", y = var)) +
    geom_line() +
    scale_size_area() +
    xlab(name)

  if (show_n) {
    plot <- plot +
      geom_point(aes(color = count)) +
      scale_colour_gradient(trans = "log10")
  }

  if (show_na) {
    plot <- plot + na_layer(x, var)
  }

  plot
}

autoplot.binsum_2d_moments <- function(x, var = "mean", show_na = TRUE, log = "") {
  x <- x[x$count > 0, ]
  miss <- is.na(x$x) + 2 * is.na(x$y)

  plot <- ggplot(x[miss == 0, ], aes_string(x = "x", y = "y")) +
    geom_tile(aes_string(fill = var)) +
    scale_fill_gradient2()

  if (show_na) {
  }

  plot <- plot + if (logv(log, "x")) scale_x_log10()
  plot <- plot + if (logv(log, "y")) scale_y_log10()

  plot
}

na_layer <- function(x, var) {
  if (x[[var]][1] == 0) return()

  xloc <- miss_poss(x$x)
  annotate("text", x = xloc, y = x[[var]][1], colour = "red", label = "NA",
    size = 3)
}

logv <- function(log, var) var %in% strsplit(log, "")[[1]]

miss_poss <- function(x) {
  rng <- frange(x)
  rng[1] - (rng[2] - rng[1]) * 0.05
}
