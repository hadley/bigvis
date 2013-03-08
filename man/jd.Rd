\name{jd}
\alias{jd}
\title{John-Draper transformation.}
\usage{
  jd(x, lambda)
}
\arguments{
  \item{x}{values to transform}

  \item{lambda}{degree of transformation:}
}
\description{
  A generalisation of the box-cox transformation that works
  for values with both positive and negative values.
}
\examples{
x <- seq(-10, 10, length = 100)
plot(x, jd(x, 0), type = "l")
plot(x, jd(x, 1), type = "l")
plot(x, jd(x, 2), type = "l")
plot(x, jd(x, -1), type = "l")
plot(x, jd(x, -2), type = "l")
}

