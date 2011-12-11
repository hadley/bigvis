\name{density_1d}
\alias{density_1d}
\title{Fast 1d kernel density estimation on regularly binned data.}
\usage{
  density_1d(bindata, bandwidth)
}
\arguments{
  \item{bindata}{Output from \code{\link{bin_1d}} or
  otherwise - needs to be in the same format.}

  \item{bandwidth}{smoothing bandwith - higher number =
  more smoothing.}
}
\description{
  Currently only provides the normal kernel, but extension
  to other kernels is straightforward (but ignore because
  not that important). The methods used in this function
  are adapted from the binned kernel density estimation
  functions in MP Wand's "KernSmooth" package, and are fast
  because they work with binned data and use the discrete
  fast fourier algorithm for efficient convolution of the
  data and kernel.
}
\examples{
x <- rnorm(1000)
bin <- bin_1d(data.frame(x), "x", 0.01)
dens <- density_1d(bin, 0.5)
plot(dens, type = "l")
}

