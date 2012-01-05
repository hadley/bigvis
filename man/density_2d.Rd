\name{density_2d}
\alias{density_2d}
\title{2d density estimation.}
\usage{
  density_2d(counts, bandwidth)
}
\description{
  2d density estimation.
}
\examples{
b2 <- bin_nd(mtcars, c("mpg", "wt"), binwidth = 0.1)
plot(b2)
s2 <- density_2d(b2, c(.5, .5))
contour(s2, add = T)
plot(s2)
}

