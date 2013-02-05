autoplot.binsum_sum <- function(x, var = "count", show_na = TRUE, log = "") {
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

autoplot.binsum_moments <- function(x, var = "mean", show_na = TRUE,
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

na_layer <- function(x, var) {
  if (x[[var]][1] == 0) return()

  xloc <- x$x[2] - diff(range(x$x, na.rm = TRUE)) * 0.05
  annotate("text", x = xloc, y = x[[var]][1], colour = "red", label = "NA",
    size = 3)
}

logv <- function(log, var) var %in% strsplit(log, "")[[1]]
