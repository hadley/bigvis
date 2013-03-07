#' Peel off low density regions of the data
#'
#' @param x condensed summary
#' @param keep (approximate) amount of data to keep.
#' @param central if \code{TRUE} peels off regions of lowest density only from
#'   the outside of the data. In 2d this works by progressively peeling off
#'   convex hull of the data. If \code{FALSE}, just removes the lowest density
#'   regions wherever they are found.
#' @export
#' @examples
#' x <- rt(1e5, df = 2)
#' y <- rt(1e5, df = 2)
#' xysum <- condense(list(bin(x, 1 / 10), bin(y, 1 / 10)))
#' plot(xysum$x, xysum$y)
#'
#' plot(peel(xysum, 0.95, central = TRUE)[1:2])
#' plot(peel(xysum, 0.90, central = TRUE)[1:2])
#' plot(peel(xysum, 0.50, central = TRUE)[1:2])
peel <- function(x, keep = 0.99, central = FALSE) {
  stopifnot(is.condensed(x))
  stopifnot(is.numeric(keep), length(keep) == 1, keep > 0, keep <= 1)
  stopifnot(is.logical(central), length(central) == 1)

  if (is.null(x$.count)) {
    stop("Can only peel objects with .count variable", call. = FALSE)
  }

  if (central) {
    peel_outer(x, keep)
  } else {
    peel_anywhere(x, keep)
  }
}

peel_anywhere <- function(x, keep) {
  ord <- order(x$.count, decreasing = TRUE)
  prop <- cumsum(x$.count[ord]) / sum(x$.count)

  # browser()
  ind <- which(prop >= keep)[1]
  x[ord < ind, , drop = FALSE]
}

peel_outer <- function(x, keep) {
  if (gcol(x) != 2) {
    stop("Outer peeling only works with 2d data", call. = FALSE)
  }

  n <- sum(x$.count)
  x <- x[order(x$.count, decreasing = TRUE), ]
  prop <- cumsum(x$.count) / n

  # Peel off smallest values on chull
  candidate <- which(prop >= keep)
  on_hull <- intersect(candidate, chull(x[1:2]))
  left <- sum(x$.count[-on_hull]) / n

  while(left >= keep && length(on_hull) > 0) {
    x <- x[-on_hull, , drop = FALSE]
    prop <- prop[-on_hull]
    candidate <- which(prop >= keep)
    on_hull <- intersect(candidate, chull(x[1:2]))
    left <- sum(x$.count[-on_hull]) / n
  }

  x
}
