\name{find_width}
\alias{find_width}
\title{Compute a reasonable default binwidth.}
\usage{
  find_width(x, nbins = 10000)
}
\arguments{
  \item{x}{a numeric vector. If a numeric vector of length
  one is supplied, it's assumed that}

  \item{nbins}{desired number of bins (approximate)}
}
\description{
  Compute a reasonable default binwidth.
}
\examples{
find_width(c(0, 5))
find_width(c(0, 5.023432))
find_width(c(0, 5.9))
}
\seealso{
  Other reasonable defaults: \code{\link{find_origin}}
}
\keyword{internal}

