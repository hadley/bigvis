is.summary_matrix <- function(x) {
  inherits(x, "summary_matrix")
}

print.summary_matrix <- function(x) {
  print(x$count)
}

as.data.frame.summary_matrix <- function(x, ...) {
  labels <- expand.grid(x$centers, KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE)
  cbind(labels, count = as.vector(x$count))  
}

dim.summary_matrix <- function(x, ...) dim(x$count)
