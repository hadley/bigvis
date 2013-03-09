#' Estimate smoothing RMSE using leave-one-out cross-valdation.
#'
#' @export
#' @examples
#' set.seed(1014)
#' # 1d -----------------------------
#' x <- rchallenge(1e4)
#' xsum <- condense(bin(x, 1 / 10))
#' cvs <- rmse_cvs(xsum)
#'
#' if (require("ggplot2")) {
#' autoplot(xsum)
#' qplot(x, err, data = cvs, geom = "line")
#' xsmu <- smooth(xsum, 1.3)
#' autoplot(xsmu)
#' autoplot(peel(xsmu))
#' }
#'
#' # 2d -----------------------------
#' y <- runif(1e4)
#' xysum <- condense(bin(x, 1 / 10), bin(y, 1 / 100))
#' cvs <- rmse_cvs(xysum, h_grid(xysum, 10))
#' if (require("ggplot2")) {
#' qplot(x, y, data = cvs, size = err)
#' }
rmse_cvs <- function(x, hs = h_grid(x), ...) {
  rmse_1 <- function(i) {
    rmse_cv(x, as.numeric(hs[i, ]), ...)
  }
  err <- vapply(seq_len(nrow(hs)), rmse_1, numeric(1))
  data.frame(hs, err)
}

rmse_cv <- function(x, h, var = summary_vars(x)[1], ...) {
  # can't smooth missing values, so drop.
  x <- x[complete.cases(x), , drop = FALSE]
  gvars <- group_vars(x)

  pred_error <- function(i) {
    out <- as.matrix(x[i, gvars, drop = FALSE])
    smu <- smooth(x[-i, , drop = FALSE], grid = out, h = h, var = var, ...)
    smu[[var]] - x[[var]][i]
  }
  err <- vapply(seq_along(nrow(x)), pred_error, numeric(1))
  sqrt(mean(err ^ 2, na.rm = TRUE))
}

#' Generate grid of plausible bandwidths for condensed summary.
#'
#' @param x a condensed summary
#' @param n number of bandwidths to generate (in each dimension)
#' @param max maximum bandwidth to generate, as multiple of binwidth.
#' @export
#' @examples
#' x <- rchallenge(1e4)
#' xsum <- condense(bin(x, 1 / 10))
#' h_grid(xsum)
#'
#' y <- runif(1e4)
#' xysum <- condense(bin(x, 1 / 10), bin(y, 1 / 100))
#' h_grid(xysum, n = 10)
h_grid <- function(x, n = 50, max = 20) {
  stopifnot(is.condensed(x))
  stopifnot(is.numeric(n), length(n) == 1, n > 0)
  stopifnot(is.numeric(max), length(max) == 1, max > 0)

  gs <- x[group_vars(x)]
  widths <- vapply(gs, attr, "width", FUN.VALUE = numeric(1))

  hs <- lapply(widths, function(w) w * seq(2, max, length = n))
  expand.grid(hs, KEEP.OUT.ATTRS = FALSE)
}
