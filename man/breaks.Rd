\name{breaks}
\alias{breaks}
\title{Compute breaks given origin and width.}
\usage{
  breaks(x, origin, binwidth)
}
\arguments{
  \item{x}{numeric vector}

  \item{origin}{bin origin}

  \item{binwidth}{bin width}
}
\description{
  Breaks are right-open, left-closed [x, y), so if
  \code{max(x)} is an integer multiple of binwidth, then we
  need one more break. This function only returns the
  left-side of the breaks.
}
\details{
  The first break is special, because it always contains
  missing values.
}
\examples{
breaks(10, origin = 0, binwidth = 1)
breaks(9.9, origin = 0, binwidth = 1)

breaks(1:10, origin = 0, binwidth = 2)
}
\keyword{internal}

