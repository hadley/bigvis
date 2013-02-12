#' Smooth 2d summary statistics
#'
#' @export
#' @examples
#' x <- runif(1e3)
#' y <- runif(1e3)
#' z <- summarise_2d(x, y, x_binwidth = 1/1000, y_binwidth = 1/1000)
#' qplot(x, y, fill = count, data = z, geom = "raster")
#' zs <- smooth_2d(z, x_n = 100, y_n = 100)
#' qplot(x, y, fill = count, data = zs, geom = "raster")
#'
#' ggplot(mapping = aes(x, y)) +
#'   geom_raster(aes(fill = count), data = z) +
#'   geom_contour(aes(z = count), data = zs, colour = "grey50")
smooth_2d <- function(summary, vars = NULL, x_bw = NULL, y_bw = NULL,
                     x_grid = NULL, y_grid = NULL, x_n = NULL, y_n = NULL,
                     standardise = TRUE) {
  stopifnot(is.binsum(summary))
  if (is.null(vars)) {
    vars <- names(summary)[-(1:2)]
    message("Smoothing ", paste(vars, collapse = ", "))
  }

  complete <- !is.na(summary$x) & !is.na(summary$y)
  miss_x   <-  is.na(summary$x) & !is.na(summary$y)
  miss_y   <- !is.na(summary$x) &  is.na(summary$y)
  miss_xy  <-  is.na(summary$x) &  is.na(summary$y)

  x_n <- x_n %||% sum(miss_y)
  x_bw <- x_bw %||% guess_bw(summary$x[miss_y])
  x_grid <- x_grid %||% guess_grid(summary$x[miss_y], x_n)

  y_n <- y_n %||% sum(miss_x)
  y_bw <- y_bw %||% guess_bw(summary$y[miss_x])
  y_grid <- y_grid %||% guess_grid(summary$y[miss_x], y_n)

  smooth_var <- function(var) {
    s <- list(
      smooth2d(summary$x[complete], summary$y[complete], summary[[var]][complete],
        x_out = x_grid, y_out = y_grid, x_sd = x_bw, y_sd = y_bw,
        standardise = standardise),
      smooth1d(summary$x[miss_y], summary[[var]][miss_y],
        x_out = x_grid, sd = x_bw, standardise = standardise),
      smooth1d(summary$y[miss_x], summary[[var]][miss_x],
        x_out = y_grid, sd = y_bw, standardise = standardise),
      summary[[var]][miss_xy]
    )
    unlist(s)
  }
  n <- length(x_grid) * length(y_grid) + length(x_grid) + length(y_grid) + 1
  smooths <- vapply(vars, smooth_var, numeric(n))

  grid <- expand.grid(x = x_grid, y = y_grid, KEEP.OUT.ATTR = FALSE)
  x <- c(grid$x, x_grid, rep(NA, length(y_grid)), NA)
  y <- c(grid$y, rep(NA, length(x_grid)), y_grid, NA)
  binsum(data.frame(x = x, y = y, smooths), type(summary))
}

guess_grid <- function(x, n = NULL) {
  g_rng <- frange(x)

  grid <- seq(g_rng[1], g_rng[2], length = n)
  if (!identical(all.equal(grid, x), TRUE)) {
    message("Generating evenly spaced grid from ", format(g_rng[1]), " to ",
      format(g_rng[2]), " with ", n, " points")
  }

  grid
}

guess_bw <- function(x) {
  bw <- min(diff(sort(x)))
  message("bandwidth set to ", format(bw))
  bw
}
