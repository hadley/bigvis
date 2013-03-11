\name{h_grid}
\alias{h_grid}
\title{Generate grid of plausible bandwidths for condensed summary.}
\usage{
  h_grid(x, n = 50, max = 20)
}
\arguments{
  \item{x}{a condensed summary}

  \item{n}{number of bandwidths to generate (in each
  dimension)}

  \item{max}{maximum bandwidth to generate, as multiple of
  binwidth.}
}
\description{
  By default, the bandwidths start at the bin width, and
  then continue up 50 (\code{n}) steps until 20
  (\code{max}) times the bin width.
}
\examples{
x <- rchallenge(1e4)
xsum <- condense(bin(x, 1 / 10))
h_grid(xsum)

y <- runif(1e4)
xysum <- condense(bin(x, 1 / 10), bin(y, 1 / 100))
h_grid(xysum, n = 10)
}
\seealso{
  Other bandwidth estimation functions:
  \code{\link{best_h}}, \code{\link{rmse_cv}},
  \code{\link{rmse_cvs}}
}

