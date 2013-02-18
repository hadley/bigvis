\name{guess_bandwidth}
\alias{guess_bandwidth}
\title{Guess the best bandwidth for smoothing a count summary.}
\usage{
  guess_bandwidth(binsum)
}
\arguments{
  \item{binsum}{a \code{\link{binsum}} object.}
}
\description{
  Guess the best bandwidth for smoothing a count summary.
}
\details{
  This uses the Sheather-Jones estimator as described in
  the reference. It was chosen because it is efficient to
  compute, appears to perform well in simulation studies,
  and a reference implementation for non-binned data
  already existed.
}
\examples{
x <- runif(1e3)
xsum <- summarise_1d(x, binwidth = 1/10)
guess_bandwidth(xsum)
}
\author{
  This code was adapted from the R and C code underlying
  \code{\link[stats]{bw.SJ}}, written Bill Venable and
  Brian Ripley.
}

