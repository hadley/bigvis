#' Condensed: an S3 class for condensed summaries.
#'
#' This object managed the properties of condensed (summarised) data frames.
#'
#' @param groups list of \code{\link{grouped}} objects
#' @param grouped,summary output from C++ condense function
#' @keywords internal
condensed <- function(groups, grouped, summary) {
  grouped <- as.data.frame(grouped)
  summary <- as.data.frame(summary)

  for (i in seq_along(groups)) {
    attr(grouped[[i]], "width") <- groups[[i]]$width()
    attr(grouped[[i]], "origin") <- groups[[i]]$origin()
  }

  names(summary) <- paste0(".", names(summary))

  df <- data.frame(grouped, summary)
  class(df) <- c("condensed", class(df))
  df
}

#' @export
#' @rdname condensed
#' @param x object to test
is.condensed <- function(x) inherits(x, "condensed")

#' @export
as.condensed <- function(x) UseMethod("as.condensed")
#' @S3method as.condensed condensed
as.condensed.condensed <- function(x) x
#' @S3method as.condensed data.frame
as.condensed.data.frame <- function(x) {
  structure(x, class = c("condensed", class(x)))
}

summary_vars <- function(x) {
  stopifnot(is.condensed(x))
  nm <- names(x)
  names(x)[grepl("^\\.", names(x))]
}

group_vars <- function(x) {
  setdiff(names(x), summary_vars(x))
}

gcol <- function(x) length(group_vars(x))

