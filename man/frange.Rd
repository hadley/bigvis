\name{frange}
\alias{frange}
\title{Efficient implementation of range.}
\usage{
  frange(x, na_rm = TRUE)
}
\arguments{
  \item{x}{a numeric vector}

  \item{na_rm}{should missing values be removed?}
}
\description{
  This is an efficient C++ implementation of range for
  numeric vectors: it avoids S3 dispatch, and computes both
  min and max in a single pass through the input.
}
\examples{
x <- runif(1e6)
system.time(range(x))
system.time(frange(x))
}

