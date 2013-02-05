#' Given a binsum object, collapse identical bins.
#'
#' This will generally not be called by the user, but is automatically called
#' when you modify the \code{x} variable in a binsum object, ensuring that there
#' are no bins with duplicate x's, collapsing the summary statistics as
#' accurately as possible.
#'
#' @param a binned summary object
#' @keywords internal
rebin <- function(x) {
  UseMethod("rebin")
}

rebin.binsum_median <- function(x) {
  if (!anyDuplicated(x)) return(x)
  message("Warning: can not rebin medians. Approximating using mean.")

  ux <- unique(x$x)
  x_id <- match(x$x, ux)
  xs <- split(seq_len(nrow(x)), x_id)

  out <- data.frame(x = ux)
  out$median <- grp_apply(xs, function(i) mean(x$median[i], na.rm = TRUE))
  binsum(out, type = type(x))
}

rebin.binsum_sum <- function(x) {
  if (!anyDuplicated(x)) return(x)


  ux <- unique(x$x)
  x_id <- match(x$x, ux)
  xs <- split(seq_len(nrow(x)), x_id)

  out <- data.frame(x = ux)
  out$count <- grp_apply(xs, function(i) sum(x$count[i]))
  if (!is.null(x$mean)) {
    out$sum <- grp_apply(xs, function(i) sum(x$sum[i]))
  }

  binsum(out, type = type(x))
}

rebin.binsum_moments <- function(x) {
  if (!anyDuplicated(x$x)) return(x)

  ux <- unique(x$x)
  x_id <- match(x$x, ux)
  xs <- split(seq_len(nrow(x)), x_id)

  out <- data.frame(x = ux)

  out$count <- grp_apply(xs, function(i) sum(x$count[i]))
  if (!is.null(x$mean)) {
    out$mean <- grp_apply(xs, function(i) weighted.mean(x$mean[i], x$count[i]))
  }

  binsum(out, type = type(x))
}

grp_apply <- function(x, f) vapply(x, f, numeric(1), USE.NAMES = FALSE)

weighted.var <- function(n_x, n_y, mu_x, mu_y, var_x, var_y) {
  n <- n_x + n_y
  1 / n * (n_x * var_x + n_y * var_y) +
    (n_x * n_y) / n ^ 2 * (mu_x ^ 2 + mu_y ^ 2 - 2 * mu_x * mu_y)
}
