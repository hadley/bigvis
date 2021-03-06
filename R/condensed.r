#' Condensed: an S3 class for condensed summaries.
#'
#' This object managed the properties of condensed (summarised) data frames.
#'
#' @section S3 methods:
#'
#' Mathematical functions with methods for \code{binsum} object will modify
#' the x column of the data frame and \code{\link{rebin}} the data, calculating
#' updated summary statistics.
#'
#' Currently methods are provided for the \code{Math} group generic,
#' logical comparison and arithmetic operators, and
#' \code{\link[plyr]{round_any}}.
#'
#' @param groups list of \code{\link{bin}}ed objects
#' @param grouped,summary output from C++ condense function
#' @keywords internal
#' @examples
#' if (require("ggplot2")) {
#'
#' x <- rchallenge(1e4)
#' xsum <- condense(bin(x, 1 / 10))
#'
#' # Basic math operations just modify the first column
#' autoplot(xsum)
#' autoplot(xsum * 10)
#' autoplot(xsum - 30)
#' autoplot(abs(xsum - 30))
#'
#' # Similarly, logical operations work on the first col
#' autoplot(xsum[xsum > 10, ])
#'}
condensed <- function(groups, grouped, summary) {
  grouped <- as.data.frame(grouped)
  summary <- as.data.frame(summary)

  for (i in seq_along(groups)) {
    grouped[[i]] <- dgrid(grouped[[i]],
      groups[[i]]$width(), groups[[i]]$origin(), groups[[i]]$nbins())
  }

  names(summary) <- paste0(".", names(summary))

  df <- data.frame(grouped, summary)
  class(df) <- c("condensed", class(df))
  df
}

#' @export
#' @rdname condensed
#' @param x object to test or coerce
is.condensed <- function(x) inherits(x, "condensed")

#' @export
#' @rdname condensed
as.condensed <- function(x) UseMethod("as.condensed")
#' @export
as.condensed.condensed <- function(x) x
#' @export
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


#' @export
Math.condensed <- function(x, ...) {
  generic <- match.fun(.Generic)
  x[[1]] <- generic(x[[1]], ...)
  rebin(x)
}

#' @export
Ops.condensed <- function(e1, e2) {
  logical_ops <- c("==", "!=", "<", "<=", ">=", ">")
  math_ops <- c("+", "-", "*", "/", "^", "%%", "%/%")

  generic <- match.fun(.Generic)
  if (.Generic %in% logical_ops)  {
    l <- generic(e1[[1]], e2)
    l[1] <- TRUE # always preserve missings
    l & !is.na(l)
  } else if (.Generic %in% math_ops) {
    e1[[1]] <- generic(e1[[1]], e2)
    rebin(e1)
  } else {
    stop(.Generic, " not supported for condensed objects", call. = FALSE)
  }
}

#' Round any method for condensed objects
#'
#' @inheritParams plyr::round_any
#' @export
#' @keywords internal
round_any.condensed <- function(x, accuracy, f = round) {
  gvars <- group_vars(x)
  x[gvars] <- lapply(x[gvars], plyr::round_any, accuracy = accuracy, f = f)
  rebin(x)
}
