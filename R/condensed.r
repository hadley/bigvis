#' Condensed data structure.
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


