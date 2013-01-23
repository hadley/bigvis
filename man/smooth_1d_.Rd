\name{smooth_1d_}
\alias{smooth_1d_}
\title{1d kernel smoothing}
\usage{
  smooth_1d_(x, z, x_out, kernel, reflect = TRUE)
}
\arguments{
  \item{x}{ordered vector of x positions}

  \item{z}{vector of values}

  \item{x_out}{vector of x positions to smooth for}

  \item{kernel}{function that when given distance between
  two locations returns the weight that should be used.}
}
\description{
  1d kernel smoothing
}

