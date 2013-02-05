\name{rebin}
\alias{rebin}
\title{Given a binsum object, collapse identical bins.}
\usage{
  rebin(x)
}
\arguments{
  \item{a}{binned summary object}
}
\description{
  This will generally not be called by the user, but is
  automatically called when you modify the \code{x}
  variable in a binsum object, ensuring that there are no
  bins with duplicate x's, collapsing the summary
  statistics as accurately as possible.
}
\keyword{internal}

