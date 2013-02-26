\name{condense}
\alias{condense}
\title{Efficient binned summaries.}
\usage{
  condense(x, z = NULL, summary = NULL, w = NULL,
    drop = NULL)
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

  \item{drop}{if \code{TRUE} only locations with data will
  be returned.  This is more efficient if the data is very
  sparse (<1\% of cells filled), and is slightly less
  efficient. Defaults to \code{TRUE} if you are condensing
  over two or more dimensions, \code{FALSE} for 1d.}
}
\description{
  Efficient binned summaries.
}
\examples{
x <- runif(1e5)
gx <- grouped(x, 0.1)
condense(gx)
}

