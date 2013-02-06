\name{summarise1d}
\alias{summarise1d}
\title{Efficient binned 1d summaries.}
\usage{
  summarise1d(x, z = NULL, summary = NULL, w = NULL,
    binwidth = NULL, origin = NULL, breaks = NULL)
}
\arguments{
  \item{x}{a numeric vector to group by}

  \item{y}{a numeric vector to summary for each group.
  Optional for some summary statistics.}

  \item{summary}{the summary statistic to use. Currently
  must be one of count, sum, mean, median or sd. If
  \code{NULL}, defaults to mean if y is present, count if
  not.}

  \item{weight}{a vector of weights.  Not currently
  supported by all summary functions.}

  \item{binwidth,origin,breaks}{Binning specification.
  Either supply binwidth and optionally origin (defaults to
  \code{min(x)}), or a vector of breaks.}
}
\description{
  Efficient binned 1d summaries.
}
\examples{
x <- runif(1e5)
summarise1d(x, binwidth = 0.1)
}

