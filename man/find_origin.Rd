\name{find_origin}
\alias{find_origin}
\title{Find the origin.}
\usage{
  find_origin(x, binwidth)
}
\arguments{
  \item{x}{numeric or integer vector}

  \item{binwidth}{binwidth}
}
\description{
  Find the origin.
}
\details{
  This algorithm implements simple heuristics for
  determining the origin of a histogram when only the
  binwidth is specified. It:

  \itemize{ \item rounds to zero, if relatively close \item
  subtracts 0.5 offset, if an x is integer \item ensures
  the origin is a multiple of the binwidth }
}
\examples{
find_origin(1:10, 1)
find_origin(1:10, 2)
find_origin(c(1, 1e6), 1)
}
\seealso{
  Other reasonable defaults: \code{\link{find_width}}
}
\keyword{internal}

