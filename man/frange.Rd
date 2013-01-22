\name{frange}
\alias{frange}
\title{Efficient implementation of range.}
\usage{
  frange(x, na_rm = TRUE)
}
\arguments{
  \item{x}{a numeric vector, or a \code{\link{ranged}}
  object}

  \item{na_rm}{should missing values be removed?}
}
\description{
  This is an efficient C++ implementation of range for
  numeric vectors: it avoids S3 dispatch, and computes both
  min and max in a single pass through the input.
}
\details{
  If \code{x} has a \code{range} attribute (e.g. it's a
  \code{\link{ranged}} object), it will be used instead of
  computing the range from scratch.
}
\examples{
x <- runif(1e6)
system.time(range(x))
system.time(frange(x))

rx <- ranged(x)
system.time(frange(rx))
}

