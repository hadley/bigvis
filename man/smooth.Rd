\name{smooth}
\alias{smooth}
\title{Smooth a condensed data frame.}
\usage{
  smooth(x, h, var = summary_vars(x)[1], grid = NULL,
    type = "mean", factor = TRUE)
}
\arguments{
  \item{x}{a condensed summary}

  \item{h}{numeric vector of bandwidths, one for each
  grouping variable in \code{x}}

  \item{var}{variable to smooth}

  \item{grid}{a data frame with the grouping colums as x.
  In order for the factored version of \code{smooth_nd} to
  work, this grid must be a superset of \code{x}.}

  \item{type}{type of smoothing to use.  Current options
  are \code{"mean"}, a kernel weighted mean;
  \code{"regression"}, a kernel weighted local regression;
  and \code{"robust_regression"}, robust kernel weighted
  local regression in the style of \code{\link{loess}}.
  Unique prefixes are also acceptable.}

  \item{factor}{if \code{TRUE} compute the n-dimensional
  smooth by a sequence of 1d smoothes. For \code{type =
  "mean"} the results are always the same grid values are
  uncorrelated (e.g. the grid is complete at every
  location); and is very approximate for \code{type =
  "robust"}.}
}
\description{
  Smooth a condensed data frame.
}
\examples{
x <- runif(1e5)
xsum <- condense(bin(x, 1 / 100))
xsmu1 <- smooth(xsum, 5 / 100)
xsmu2 <- smooth(xsum, 5 / 100, factor = FALSE)

# More challenging distribution
x <- rchallenge(1e4)
xsum <- condense(bin(x, 0.1))
xsmu <- smooth(xsum, 1)

plot(xsum$x, xsum$.count, type = "l")
lines(xsmu$x, xsmu$.count, col = "red")

xsmu2 <- smooth(xsum, 1, type = "regress")
plot(xsmu$x, xsmu$.count, type = "l", xlim = c(0, 50))
lines(xsmu2$x, xsmu2$.count, col = "red")
# Note difference in tails
}

