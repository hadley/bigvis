#' Transform condensed objects, collapsing unique bins.
#'
#' @details
#' You don't need to use \code{rebin} if you use transform: it will
#' automatically rebin for you.  You will need to use it if you manually
#' transform any grouping variables.
#'
#' @param data,`_data` a condensed summary
#' @param ... named arguments evaluated in the context of the data
#' @usage \\method{transform}{condensed}(`_data`, ...)
#' @keywords internal
#' @examples
#' x <- runif(1e4, -1, 1)
#' xsum <- condense(bin(x, 1 / 50))
#'
#' # Transforming by hand: must use rebin
#' xsum$x <- abs(xsum$x)
#' rebin(xsum)
#' if (require("ggplot2")) {
#'   autoplot(xsum) + geom_point()
#'   autoplot(rebin(xsum)) + geom_point()
#' }
#'
#' #' Transforming with transform
#' y <- x ^ 2 + runif(length(x), -0.1, 0.1)
#' xysum <- condense(bin(x, 1 / 50), z = y)
#' xysum <- transform(xysum, x = abs(x))
#' if (require("ggplot2")) {
#'   autoplot(xysum)
#' }
#' @export
#' @method transform condensed
transform.condensed <- function(`_data`, ...) {
  df <- transform.data.frame(`_data`, ...)
  rebin(as.condensed(df))
}

#' @export
#' @rdname transform.condensed
rebin <- function(data) {
  stopifnot(is.condensed(data))

  old_g <- data[group_vars(data)]
  old_g[] <- lapply(old_g, zapsmall, digits = 3)
  ids <- id(old_g, drop = TRUE)
  if (!anyDuplicated(ids)) return(data)

  old_s <- data[summary_vars(data)]
  new_s <- lapply(names(old_s), function(var) rebin_var(old_s, ids, var))
  names(new_s) <- names(old_s)

  uids <- !duplicated(ids)
  new_g <- old_g[uids, , drop = FALSE]
  ord <- order(ids[uids], na.last = FALSE)

  as.condensed(data.frame(new_g[ord, , drop = FALSE], new_s))
}

rebin_var <- function(df, ids, var) {
  stopifnot(is.data.frame(df))
  stopifnot(is.integer(ids), length(ids) == nrow(df))
  stopifnot(is.character(var), length(var) == 1, var %in% names(rebinners))

  rows <- split(seq_len(nrow(df)), ids)
  f <- rebinners[[var]]

  vapply(rows, function(i) f(df[i, , drop = FALSE]), numeric(1),
    USE.NAMES = FALSE)
}

rebinners <- list(
  .median = function(df) mean(df$.median, na.rm = TRUE),
  .sum = function(df) sum(df$.sum, na.rm = TRUE),
  .count = function(df) sum(df$.count, na.rm = TRUE),
  .mean = function(df) {
    if (is.null(df$.count)) {
      mean(df$.mean, na.rm = TRUE)
    } else {
      weighted.mean(df$.mean, df$.count)
    }
  }
)
