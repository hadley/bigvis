\name{binsum}
\alias{binsum}
\alias{is.binsum}
\title{Create/test for binned summary objects.}
\usage{
  binsum(df, type)

  is.binsum(x)
}
\description{
  Create/test for binned summary objects.
}
\section{Methods}{
  Mathematical functions with methods for \code{binsum}
  object will modify the x column of the data frame and
  \code{\link{rebin}} the data, calculating updated summary
  statistics.

  Currently methods are provided for the \code{Math} group
  generic, logical comparison and arithmetic operators, and
  \code{\link[plyr]{round_any}}.
}
\keyword{internal}

