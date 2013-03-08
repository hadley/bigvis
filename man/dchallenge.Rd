\name{dchallenge}
\alias{dchallenge}
\alias{rchallenge}
\title{Density and random number generation functions for a challenging
distribution.}
\usage{
  dchallenge(x)

  rchallenge(n)
}
\arguments{
  \item{x}{values to evaluate pdf at}

  \item{n}{number of random samples to generate}
}
\description{
  This is a 1/3-2/3 mixture of a t-distribution with 2
  degrees of freedom centered at 15 and scaled by 2, and a
  gamma distribution with shape 2 and rate 1/3. (The
  t-distribution is windsorised at 0, but this has
  negligible effect.) This distribution is challenging
  because it mixes heavy tailed and asymmetric
  distributions.
}
\examples{
plot(dchallenge, xlim = c(-5, 60), n = 500)

x <- rchallenge(1e4)
hist(x, breaks = 1000)
xsum <- condense(bin(x, 0.1))
plot(xsum$x, xsum$.count, type = "l")
xsmu <- smooth(xsum, 0.3)
plot(xsmu$x, xsmu$.count, type = "l")
plot(xsmu$x, xsmu$.count, type = "l", xlim = c(0, 30))
}

