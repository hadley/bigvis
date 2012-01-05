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
  cbind(labels, value = as.vector(x$data))  
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
    if (max(x$data) == 0) return()
    image(x$centers[[2]], x$centers[[1]], t(x$data), useRaster = TRUE, 
      xlab = names(x$centers)[2], ylab = names(x$centers)[1],
      col = grey(seq(1, 0, length = 20)), zlim = c(0, max(x$data)))
  } else {
    stop("Don't know how to plot more than 2d")
  }
  
}

#' @S3method lines binned_summary 
lines.binned_summary <- function(x, ...) {
  lines(x$centers[[1]], x$data, ...)  
}

#' @S3method contour binned_summary 
contour.binned_summary <- function(x, ..., drawlabels = FALSE) {
  contour(x$centers[[2]], x$centers[[1]], t(x$data), ..., 
    drawlabels = drawlabels)
}

#' @S3method points binned_summary
points.binned_summary <- function(x, ...) {
  non_zero <- x$data > 1e-6
  points(x$centers[[1]][non_zero], x$data[non_zero], ...)  
}

#' @S3method [ binned_summary 
"[.binned_summary" <- function(x, ..., drop = TRUE) {
  call <- match.call()
  idx <- as.list(call[-1])
  idx$x <- NULL
  idx$drop <- NULL
  stopifnot(length(idx) == length(dim(x)))
  
  
  one_d <- function(list, call) {
    eval(as.call(c(as.name("["), as.name("list"), call)))
  }
  x$centers <- mapply(one_d, x$centers, idx, SIMPLIFY = FALSE)
  x$nbins <- lapply(x$centers, length)

  single <- vapply(x$centers, length, integer(1)) == 1L
  if (drop && any(single)) {
    x$binwidth <- x$binwidth[!single]
    x$origin <- x$origin[!single]
    x$nbin <- x$nbin[!single]
    x$centers <- x$centers[!single]

    x$data <- array(x$data[...], unlist(x$nbins[!single]))
    dimnames(x$data) <- x$centers
  } else {
    x$data <- x$data[..., drop = FALSE]
  }

  x
}

#' @S3method as.matrix binned_summary
as.matrix.binned_summary <- function(x, ...) {
  stopifnot(length(dim(x)) == 2) 
  x$data  
}
#' @S3method as.array binned_summary
as.array.binned_summary <- function(x, ...) x$data
