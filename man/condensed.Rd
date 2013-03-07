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
\section{S3 methods}{
  Mathematical functions with methods for \code{binsum}
  object will modify the x column of the data frame and
  \code{\link{rebin}} the data, calculating updated summary
  statistics.

  Currently methods are provided for the \code{Math} group
  generic, logical comparison and arithmetic operators, and
  \code{\link[plyr]{round_any}}.
}
\examples{
if (require("ggplot2")) {

x <- rchallenge(1e4)
xsum <- condense(bin(x, 1 / 10))

# Basic math operations just modify the first column
autoplot(xsum)
autoplot(xsum * 10)
autoplot(xsum - 30)
autoplot(abs(xsum - 30))

# Similarly, logical operations work on the first col
autoplot(xsum[xsum > 10, ])
}
}
\keyword{internal}

