\name{density_2d}
\alias{density_2d}
\title{2d density estimation.}
\usage{
  density_2d(counts, bandwidth)
}
\arguments{
  \item{counts}{output from \code{\link{bin_2d}}}

  \item{bandwidth}{smoothing parameter, a vector of length
  two. Should be relatively large compared to binwidth,
  otherwise data will not be smoothed.}
}
\description{
  The methods used in this function are adapted from the
  binned kernel density estimation functions as used in MP
  Wand's \pkg{KernSmooth}.
}
\examples{
b2 <- bin_nd(mtcars, c("mpg", "wt"), binwidth = 0.1)
plot(b2)
s2 <- density_2d(b2, c(.5, .5))
contour(s2, add = T)
plot(s2)

time <- bin_nd(movies, c("year", "length"))
plot(time)
time <- time[, 1:150]
plot(time)
plot(density_2d(time, c(1, 1)))
}

