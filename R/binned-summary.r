is.binned_summary <- function(x) {
  inherits(x, "binned_summary")
}

#' @S3method print binned_summary
print.binned_summary <- function(x) {
  print(x$data)
}

#' @S3method as.data.frame binned_summary
as.data.frame.binned_summary <- function(x, ...) {
  labels <- expand.grid(x$centers, KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE)
  cbind(labels, count = as.vector(x$data))  
}

#' @S3method dim binned_summary
dim.binned_summary <- function(x, ...) dim(x$data)

#' @S3method plot binned_summary
plot.binned_summary <- function(x, ...) {
  dims <- length(dim(x))
  if (dims == 1L) {
    plot(x$centers[[1]], x$data, type = "l", 
      xlab = names(x$centers)[1], ylab = "data")
  } else if (dims == 2L) {
    image(x$centers[[1]], x$centers[[2]], x$data, useRaster = TRUE)
  } else {
    stop("Don't know how to plot more than 2d")
  }
  
}

#' @S3method binned_summary lines
lines.binned_summary <- function(x, ...) {
  lines(x$centers[[1]], x$data, ...)  
}

#' @S3method binned_summary points
points.binned_summary <- function(x, ...) {
  non_zero <- x$data > 1e-6
  points(x$centers[[1]][non_zero], x$data[non_zero], ...)  
}

