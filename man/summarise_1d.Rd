\name{summarise_1d}
\alias{summarise_1d}
\title{Efficient binned 1d summaries.}
\usage{
  summarise_1d(x, z = NULL, summary = NULL, w = NULL)
}
\arguments{
  \item{x}{a group object created by \code{\link{grouped}}}

  \item{y}{a numeric vector to summary for each group.
  Optional for some summary statistics.}

  \item{summary}{the summary statistic to use. Currently
  must be one of count, sum, mean, median or sd. If
  \code{NULL}, defaults to mean if y is present, count if
  not.}

  \item{weight}{a vector of weights.  Not currently
  supported by all summary functions.}
}
\description{
  Efficient binned 1d summaries.
}
\examples{
x <- runif(1e5)
gx <- grouped(x, 0.1)
summarise_1d(gx)
}

