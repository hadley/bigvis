\name{smooth_1d_normal}
\alias{smooth_1d_normal}
\title{1d normal kernel smoothing.}
\usage{
  smooth_1d_normal(x, z, x_out, sd)
}
\arguments{
  \item{x}{ordered vector of x positions}

  \item{z}{vector of values}

  \item{x_out}{vector of x positions to produce smoothed
  values}

  \item{sd}{standard deviation of normal kernel (the
  bandwidth of the smoother)}
}
\description{
  This is a variant of \code{\link{density}} for smoothing
  with normal kernels, where both the input and the output
  can be irregular locations.
}
\keyword{internal}

