\name{weighted.median}
\alias{weighted.median}
\title{Compute the median of weighted data.}
\usage{
  weighted.median(x, w, na.rm = FALSE)
}
\arguments{
  \item{x}{numeric vector of observations}

  \item{w}{integer vector of weights, representing the
  number of time each \code{x} was observed}

  \item{na.rm}{If \code{TRUE} will automatically remove
  missing values in \code{x} or \code{w}.}
}
\description{
  Compute the median of weighted data.
}
\details{
  This is a simple wrapper around
  \code{\link{weighted.quantile}}
}
\examples{
x <- runif(200)
w <- rpois(200, 5) + 1

median(x)
weighted.median(x, w)
}

