is.summary_matrix <- function(x) {
  inherits(x, "summary_matrix")
}

#' @S3method print summary_matrix
print.summary_matrix <- function(x) {
  print(x$count)
}

#' @S3method as.data.frame summary_matrix
as.data.frame.summary_matrix <- function(x, ...) {
  labels <- expand.grid(x$centers, KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE)
  cbind(labels, count = as.vector(x$count))  
}

#' @S3method dim summary_matrix
dim.summary_matrix <- function(x, ...) dim(x$count)
