"%||%" <- function(x, y) if (is.null(x)) y else x

last <- function(x) x[length(x)]

"%contains%" <- function(df, var) {
  var %in% names(df)
}
