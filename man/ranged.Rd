\name{ranged}
\alias{ranged}
\title{A S3 class for caching the range of a vector}
\usage{
  ranged(x, range = frange(x, na_rm = TRUE))
}
\arguments{
  \item{x}{a numeric vector}

  \item{range}{the range of the vector (excluding missing
  values), if known.  If unknown, it will be computed with
  \code{\link{frange}}, a fast C++ implementation of
  \code{\link{range}}.}
}
\description{
  For best performance, you may want to run copy and paste
  the contents of this function into your function, to
  avoid making any copies of \code{x}.
}
\examples{
x <- runif(1e6)
y <- ranged(x)
range(y)

# Modifications to the class currently destroy the cache
y[1] <- 10
max(y)
class(y)
}

