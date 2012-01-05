is.summary_matrix <- function(x) {
  inherits(x, "summary_matrix")
}

#' @S3method print summary_matrix
print.summary_matrix <- function(x) {
  print(x$data)
}

#' @S3method as.data.frame summary_matrix
as.data.frame.summary_matrix <- function(x, ...) {
  labels <- expand.grid(x$centers, KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE)
  cbind(labels, count = as.vector(x$data))  
}

#' @S3method dim summary_matrix
dim.summary_matrix <- function(x, ...) dim(x$data)

#' @S3method plot summary_matrix
plot.summary_matrix <- function(x, ...) {
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

#' @S3method summary_matrix lines
lines.summary_matrix <- function(x, ...) {
  lines(x$centers[[1]], x$data, ...)  
}

#' @S3method summary_matrix points
points.summary_matrix <- function(x, ...) {
  non_zero <- x$data > 1e-6
  points(x$centers[[1]][non_zero], x$data[non_zero], ...)  
}
