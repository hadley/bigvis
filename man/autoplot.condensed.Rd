\name{autoplot.condensed}
\alias{autoplot.condensed}
\title{Autoplot method for condensed.}
\usage{
  \method{autoplot}{condensed} (x,
    var = last(summary_vars(x)), ...)
}
\description{
  Autoplot method for condensed.
}
\examples{
if (require("ggplot2")) {

# 1d summaries -----------------------------
x <- rchallenge(1e3)
z <- x + rt(length(x), df = 2)
xsum <- condense(bin(x, 0.1))
zsum <- condense(bin(x, 0.1), z)

autoplot(xsum)
autoplot(peel(xsum))

autoplot(zsum)
autoplot(peel(zsum, keep = 1))
autoplot(peel(zsum))

}
}

