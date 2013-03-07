\name{condensed}
\alias{condensed}
\alias{is.condensed}
\title{Condensed: an S3 class for condensed summaries.}
\usage{
  condensed(groups, grouped, summary)

  is.condensed(x)
}
\arguments{
  \item{groups}{list of \code{\link{grouped}} objects}

  \item{grouped,summary}{output from C++ condense function}

  \item{x}{object to test}
}
\description{
  This object managed the properties of condensed
  (summarised) data frames.
}
\keyword{internal}

