\name{density_1d}
\alias{density_1d}
\title{Fast 1d kernel density estimation on regularly binned data.}
\usage{
  density_1d(counts, bandwidth)
}
\arguments{
  \item{counts}{Output from \code{\link{bin_1d}} or
  otherwise - needs to be in the same format.}

  \item{bandwidth}{smoothing bandwith - higher number =
  more smoothing.}
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
bin <- bin_nd(mtcars, "mpg", 0.01)
dens <- density_1d(bin, 0.5)
plot(dens)
lines(bin, col = "red")

dens <- density_1d(bin, 0.25)
plot(dens)
lines(bin, col = "red")
}

