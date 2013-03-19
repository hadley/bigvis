\name{dgrid}
\alias{dgrid}
\alias{is.dgrid}
\title{dgrid: an S3 class for data grids}
\usage{
  dgrid(x, width, origin = 0, nbins = NULL)

  is.dgrid(x)
}
\arguments{
  \item{x}{a numeric vector to test or coerce.}

  \item{width}{bin width}

  \item{origin}{bin origins}

  \item{nbins}{number of bins}
}
\description{
  dgrid: an S3 class for data grids
}
\examples{
g <- dgrid(0:10 + 0.5, width = 1)
range(g)
as.integer(g)
}

