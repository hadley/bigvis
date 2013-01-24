\name{smooth1d}
\alias{smooth1d}
\title{Smooth 1d summary statistics}
\usage{
  smooth1d(summary, bw, var = names(summary)[2],
    grid = NULL, n = nrow(summary), reflect = TRUE)
}
\arguments{
  \item{summary}{summary statistics produced by
  \code{\link{summary1d}}}

  \item{bw}{smoothing bandwidth/standard deviation}

  \item{var}{variable to smooth}

  \item{grid,n}{A grid of locations to produced smoothed
  values. If \code{NULL}, the default, will be an evenly
  spaced grid of \code{n} points across the the range of
  \code{x}.}
}
\description{
  Smooth 1d summary statistics
}
\examples{
x <- runif(1e5)
xsum <- summarise1d(x, binwidth = 1/100)
xsmu <- smooth1d(xsum, 1/100)

plot(xsum)
lines(xsmu)
}

