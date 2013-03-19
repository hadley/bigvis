#' Find "best" smoothing parameter using leave-one-out cross validation.
#'
#' Minimises the leave-one-out estimate of root mean-squared error to find
#' find the "optimal" bandwidth for smoothing.
#'
#' L-BFGS-B optimisation is used to constrain the bandwidths to be greater
#' than the binwidths: if the bandwidth is smaller than the binwidth it's
#' impossible to compute the rmse because no smoothing occurs. The tolerance
#' is set relatively high for numerical optimisation since the precise choice
#' of bandwidth makes little difference visually, and we're unlikely to have
#' sufficient data to make a statistically significant choice anyway.
#'
#' @param x condensed summary to smooth
#' @param h_init initial values of bandwidths to start search out. If not
#'  specified defaults to 5 times the binwidth of each variable.
#' @param ... other arguments (like \code{var}) passed on to
#'  \code{\link{rmse_cv}}
#' @param tol numerical tolerance, defaults to 1\%.
#' @param control additional control parameters passed on to \code{\link{optim}}
#'   The most useful argument is probably trace, which makes it possible to
#'   follow the progress of the optimisation.
#' @family bandwidth estimation functions
#' @return a single numeric value representing the bandwidth that minimises
#'   the leave-one-out estimate of rmse. Vector has attributes
#'   \code{evaluations} giving the number of times the objective function
#'   was evaluated. If the optimisation does not converge, or smoothing is not
#'   needed (i.e. the estimate is on the lower bounds), a warning is thrown.
#' @export
#' @examples
#' x <- rchallenge(1e4)
#' xsum <- condense(bin(x, 1 / 10))
#' h <- best_h(xsum, control = list(trace = 1, REPORT = 1))
#' h <- best_h(xsum)
#'
#' if (require("ggplot2")) {
#' autoplot(xsum)
#' autoplot(smooth(xsum, h))
#' }
best_h <- function(x, h_init = NULL, ..., tol = 1e-2, control = list()) {
  stopifnot(is.condensed(x))

  gvars <- group_vars(x)
  widths <- vapply(x[gvars], attr, "width", FUN.VALUE = numeric(1))
  h_init <- h_init %||% widths * 5
  stopifnot(is.numeric(h_init), length(h_init) == length(gvars))

  stopifnot(is.list(control))
  control <- modifyList(list(factr = tol / .Machine$double.eps), control)

  # Optimise
  rmse <- function(h) {
    rmse_cv(x, h, ...)
  }
  res <- optim(h_init, rmse, method = "L-BFGS-B", lower = widths * 1.01,
    control = control)
  h <- unname(res$par)

  # Feedback
  if (res$convergence != 0) {
    warning("Failed to converge: ", res$message, call. = FALSE)
  } else if (rel_dist(h, widths) < 1e-3) {
    warning("h close to lower bound: smoothing not needed", call. = FALSE)
  }
  structure(h, evaluations = res$counts[1])
}

rel_dist <- function(x, y) {
  mean(abs(x - y) / abs(x + y))
}

#' Generate grid of plausible bandwidths for condensed summary.
#'
#' By default, the bandwidths start at the bin width, and then continue
#' up 50 (\code{n}) steps until 20 (\code{max}) times the bin width.
#'
#' @param x a condensed summary
#' @param n number of bandwidths to generate (in each dimension)
#' @param max maximum bandwidth to generate, as multiple of binwidth.
#' @family bandwidth estimation functions
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
