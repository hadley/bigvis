\name{smooth_1d_normal}
\alias{smooth_1d_normal}
\title{1d normal kernel smoothing.}
\usage{
  smooth_1d_normal(x, z, x_out, sd, standardise = TRUE)
}
\arguments{
  \item{x}{ordered vector of x positions}

  \item{z}{vector of values}

  \item{x_out}{vector of x positions to produce smoothed
  values}

  \item{sd}{standard deviation of normal kernel (the
  bandwidth of the smoother)}

  \item{standardise}{if \code{TRUE}, divides the weighted
  sum at each location by the sum of the weights. This is
  usually what you want for pre-binned data, as it
  interpolates between the points, rather than
  redistributing the density.  Any locations in
  \code{x_out} that are more than 4 standard deviations
  away from \code{x_in} will be \code{NaN}.}
}
\description{
  This is a variant of \code{\link{density}} for
  calculating weighted means or weighted sums, with weights
  determined by a normal kernel. Both the input and the
  output can have irregular locations.
}
\keyword{internal}

