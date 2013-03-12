\name{transform.condensed}
\alias{rebin}
\alias{transform.condensed}
\title{Transform condensed objects, collapsing unique bins.}
\usage{
  \\method{transform}{condensed}(`_data`, ...)

  rebin(`_data`)
}
\arguments{
  \item{data,`_data`}{a condensed summary}

  \item{...}{named arguments evaluated in the context of
  the data}
}
\description{
  Transform condensed objects, collapsing unique bins.
}
\details{
  You don't need to use \code{rebin} if you use transform:
  it will automatically rebin for you.  You will need to
  use it if you manually transform any grouping variables.
}
\examples{
x <- runif(1e4, -1, 1)
xsum <- condense(bin(x, 1 / 50))

# Transforming by hand: must use rebin
xsum$x <- abs(xsum$x)
rebin(xsum)
if (require("ggplot2")) {
  autoplot(xsum) + geom_point()
  autoplot(rebin(xsum)) + geom_point()
}

#' Transforming with transform
y <- x ^ 2 + runif(length(x), -0.1, 0.1)
xysum <- condense(bin(x, 1 / 50), z = y)
xysum <- transform(xysum, x = abs(x))
if (require("ggplot2")) {
  autoplot(xysum)
}
}
\keyword{internal}

