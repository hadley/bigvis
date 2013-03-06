#' Smooth a condensed data frame.
#'
#' @param grid a data frame with the grouping colums as x.  In order for the
#'   factored version of \code{smooth_nd} to work, this grid must be a superset
#'   of \code{x}.
#' @export
#' @examples
#' x <- runif(1e5)
#' xsum <- condense(bin(x, 1 / 100))[-1, ]
#' xsmu1 <- smooth(xsum, 5 / 100)
#' xsmu2 <- smooth(xsum, 5 / 100, factor = FALSE)
smooth <- function(x, h, var = summary_vars(x)[1], grid = NULL, factor = TRUE) {
  stopifnot(is.condensed(x))
  stopifnot(is.numeric(h), all(h > 0))

  grid_in <- as.matrix(x[group_vars(x)])
  grid_out <- grid %||% grid_in

  z <- x[[var]]
  w <- if (var != ".count" && !is.null(var$.count)) var$.count else numeric()

  if (factor) {
    for(i in 1:ncol(grid_in)) {
      # smooth_nd_1 is a C++ function, so var is 0 indexed
      z <- smooth_nd_1(grid_in, z, w, grid_out, var = i - 1, h = h[i])
    }
  } else {
    z <- smooth_nd(grid_in, z, w, grid_out, h)
  }

  out <- data.frame(grid_out)
  out[[var]] <- z
  out
}
