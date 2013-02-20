#' Compute a weighted variance or standard deviation of a vector.
#'
#' @details
#' Note that unlike the base R \code{\link{var}} function, these functions only
#' work with individual vectors not matrices or data frames.
#'
#' @family weighted statistics
#' @seealso \code{\link[stats]{weighted.mean}}
#' @param x numeric vector of observations
#' @param w integer vector of weights, representing the number of
#'  time each \code{x} was observed
#' @export
#' @examples
#' x <- c(1:5)
#' w <- rpois(5, 5) + 1
#' y <- x[rep(seq_along(x), w)]
#' weighted.var(x, w)
#' var(y)
#'
#' stopifnot(all.equal(weighted.var(x, w), var(y)))
weighted.var <- function(x, w = NULL, na.rm = FALSE) {
  if (na.rm) {
    na <- is.na(x) | is.na(w)
    x <- x[!na]
    w <- w[!na]
  }

  sum(w * (x - weighted.mean(x, w)) ^ 2) / (sum(w) - 1)
}

#' @export
#' @rdname weighted.var
weighted.sd <- function(x, w, na.rm = TRUE) sqrt(weighted.var(x, w, na.rm = TRUE))

#' A weighted ecdf function.
#'
#' An extension of the base \code{\link[stats]{ecdf}} function which works
#' with weighted data.
#'
#' @section S3 methods:
#' The \code{ecdf} class has methods for \code{\link{plot}},
#' \code{\link{lines}}, \code{\link{summary}} and \code{\link{quantile}}.
#' \code{\link{quantile}} does not currently correctly compute values for
#' weighted ecdfs.
#'
#' @inheritParams weighted.var
#' @family weighted statistics
#' @seealso \code{\link[stats]{weighted.mean}}
#' @export
#' @examples
#' x <- runif(200)
#' w <- rpois(200, 5) + 1
#'
#' e <- weighted.ecdf(x, w)
#' plot(e)
#' summary(e)
#'
#' y <- x[rep(seq_along(x), w)]
#' plot(ecdf(y))
weighted.ecdf <- function(x, w) {
  stopifnot(length(x) == length(w))
  stopifnot(anyDuplicated(x) == 0)

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  n <- sum(w)
  wts <- cumsum(w / n)

  f <- approxfun(x, wts, method = "constant", yleft = 0, yright = 1, f = 0)
  class(f) <- c("wecdf", "ecdf", "stepfun", class(f))
  attr(f, "call") <- sys.call()
  environment(f)$nobs <- n
  f
}

#' Compute quantiles of weighted data.
#'
#' @details
#' Currently only implements the type 7 algorithm, as described in
#' \code{\link{quantile}}. Based on \code{\link{quantile}} written by R-core.
#'
#' @inheritParams weighed.var
#' @param probs numeric vector of probabilities between 0 and 1
#' @param na.rm If \code{TRUE} will automatically remove missing values
#'   in \code{x} or \code{w}.
#' @family weighted statistics
#' @examples
#' x <- runif(200)
#' w <- rpois(200, 5) + 1
#' weighted.quantile(x, w)
weighted.quantile <- function (x, w, probs = seq(0, 1, 0.25), na.rm = FALSE) {
  stopifnot(length(x) == length(w))
  na <- is.na(x) | is.na(w)
  if (any(na)) {
    if (na.rm) {
      x <- x[!na]
      w <- w[!na]
    } else {
      stop("Missing values not allowed when na.rm is FALSE", call. = FALSE)
    }
  }

  # Ensure x and w in ascending order of x
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  # Find closest x just below and above index
  n <- sum(w)
  index <- 1 + (n - 1) * probs
  j <- floor(index)

  wts <- cumsum(w)
  lo <- x[lowerBound(j, wts)]     # X_j
  hi <- x[lowerBound(j + 1, wts)]

  g <- index - j
  ifelse(lo == hi, lo, (1 - g) * lo + g * hi)
}
# Q[i](p) = (1 - g) x[j] + g x[j+1]
# j = floor(np + m)
# g = np + m - j
#
# For type 7:
#   m = 1 - p =>
#   j = floor(1 + (n - 1) * p)
#   g = (np + 1 - p) - floor(1 + (n - 1) * p)
