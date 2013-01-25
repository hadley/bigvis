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
  Currently only provides the normal kernel, but extension
  to other kernels is straightforward (but ignored because
  they don't make that much difference to the final
  result).
}
\details{
  The methods used in this function are adapted from the
  ideas of binned kernel density in MP Wand's "KernSmooth"
  package, but rather than using the discrete discrete fast
  fourier algorithm, they do convolution using C-level
  loops. This is more efficient when the matrices to be
  convolved are rather different sizes.
}
\examples{
x <- runif(1e5)
xsum <- summarise1d(x, binwidth = 1/100)
xsmu <- smooth1d(xsum, 1/100)

plot(xsum)
lines(xsmu)
}

