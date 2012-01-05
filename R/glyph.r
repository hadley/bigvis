#' Create the data needed to generate a glyph plot.
#'
#' @param data A data frame containing variables named in \code{x_major},
#'   \code{x_minor}, \code{y_major} and \code{y_minor}.
#' @param x_major,x_minor,y_major,y_minor The name of the variable (as a
#'   string) for the major and minor x and y axes.  Together, each unique
#    combination of \code{x_major} and \code{y_major} specifies a grid cell.
#'   \code{y_minor} defaults to \code{value} - the value of the summary in
#'   each bin.
#' @param height,width The height and width of each glyph. Defaults to 95\% of
#'   the binwidth of each dimension. Specify the width absolutely by supplying
#'   a numeric vector of length 1, or relative to the binwidth of the data by
#'   using \code{\link{rel}}.
#' @export
#' time <- bin_nd(movies, c("year", "length"), c(10, 5))
#' time <- time[, 1:40]
#' plot(time)
#'
#' library(ggplot2)
#' ratings <- bin_nd(movies, c("year", "length", "rating"), c(10, 5, 0.2),
#'   c(1900, 0, 1))
#' ratings <- ratings[, 1:40, ]
#' 
#' ratings_df <- glyphs(ratings, "year", "rating", "length")
#' qplot(gx, gy, data = ratings_df, geom = "line", group = gid)
#'
#' ratings_df <- glyphs(standardise(ratings, 1), "year", "rating", "length")
#' qplot(gx, gy, data = ratings_df, geom = "line", group = gid)
#'
#' ratings_df <- glyphs(standardise(ratings, 2), "year", "rating", "length")
#' qplot(gx, gy, data = ratings_df, geom = "line", group = gid)
#'
#' ratings_df <- glyphs(standardise(ratings, c(1, 2)), "year", "rating", "length")
#' qplot(gx, gy, data = ratings_df, geom = "line", group = gid)
#'
#' ratings_df$n <- ave(ratings_df$value, ratings_df$gx, ratings_df$gy, FUN = sum)
#' 
glyphs <- function(summary, x_major, x_minor, y_major, y_minor = "value", height = rel(0.95), width = rel(0.95)) {
  stopifnot(is.binned_summary(summary))
  data <- as.data.frame(summary)
  data$gid <- interaction(data[[x_major]], data[[y_major]], drop = TRUE)
  
  if (is.rel(width)) {
    width <- summary$binwidth[[x_major]] * unclass(width)
    message("Using width ", format(width, digits = 3))
  }
    
  if (is.rel(height)) {
    height <- summary$binwidth[[y_major]] * unclass(height)
    message("Using height ", format(height, digits = 3))
  }
  
  data$gx <- data[[x_major]] + rescale11(data[[x_minor]]) * width / 2
  data$gy <- data[[y_major]] + rescale11(data[[y_minor]]) * height / 2 
  
  structure(data, 
    width = width, height = height, 
    x_major = x_major, y_major = y_major,
    class = c("glyphplot", "data.frame"))
}

#' Create reference boxes for a glyph plot
#' @export
ref_boxes <- function(data, fill = NULL) {
  stopifnot(is.glyphplot(data))
  glyph <- attributes(data)
  cells <- unique(data[c(glyph$x_major, glyph$y_major)])
  
  data.frame(
    xmin = cells[[glyph$x_major]] - glyph$width / 2, 
    xmax = cells[[glyph$x_major]] + glyph$width / 2, 
    ymin = cells[[glyph$y_major]] - glyph$height / 2,
    ymax = cells[[glyph$y_major]] + glyph$height / 2) 
}

# Glyph plot class -----------------------------------------------------------

glyphplot <- function(data, width, height, polar, x_major, y_major) {
  structure(data, 
    width = width, height = height, polar = polar, 
    x_major = x_major, y_major = y_major,
    class = c("glyphplot", "data.frame"))  
}
is.glyphplot <- function(x) inherits(x, "glyphplot")

"[.glyphplot" <- function(x, ...) {
  glyphplot(NextMethod(), 
    width = attr(x, "width"), height = attr(x, "height"),
    x_major = attr(x, "x_major"), y_major = attr(x, "y_major"),
    polar = attr(x, "polar"))
}

print.glyphplot <- function(x, ...) {
  width <- format(attr(x, "width"), digits = 3)
  height <- format(attr(x, "height"), digits = 3)

  cat("Cartesian glyphs: \n")
  cat("  Size: [", width, ", ", height,  "]\n", sep = "")
  cat("  Major axes: ", attr(x, "x_major"), ", ", attr(x, "y_major"), "\n\n", 
    sep = "")
  print.data.frame(x)
}

# Relative dimensions --------------------------------------------------------

rel <- function(x) {
  structure(x, class = "rel")
}
print.rel <- function(x, ...) print(noquote(paste(x, " *", sep = "")))
is.rel <- function(x) inherits(x, "rel")

# Rescaling functions --------------------------------------------------------

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale11 <- function(x) 2 * rescale01(x) - 1
