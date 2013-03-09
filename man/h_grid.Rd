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
  Generate grid of plausible bandwidths for condensed
  summary.
}
\examples{
x <- rchallenge(1e4)
xsum <- condense(bin(x, 1 / 10))
h_grid(xsum)

y <- runif(1e4)
xysum <- condense(bin(x, 1 / 10), bin(y, 1 / 100))
h_grid(xysum, n = 10)
}

