\name{smooth}
\alias{smooth}
\title{Smooth a condensed data frame.}
\usage{
  smooth(x, h, var = summary_vars(x)[1], grid = NULL,
    type = "mean", factor = TRUE)
}
\arguments{
  \item{grid}{a data frame with the grouping colums as x.
  In order for the factored version of \code{smooth_nd} to
  work, this grid must be a superset of \code{x}.}
}
\description{
  Smooth a condensed data frame.
}
\examples{
x <- runif(1e5)
xsum <- condense(bin(x, 1 / 100))[-1, ]
xsmu1 <- smooth(xsum, 5 / 100)
xsmu2 <- smooth(xsum, 5 / 100, factor = FALSE)

# More challenging distribution
x <- rchallenge(1e4)
xsum <- condense(bin(x, 0.1))[-1, ]
xsmu <- smooth(xsum, 1)

plot(xsum$x, xsum$.count, type = "l")
lines(xsmu$x, xsmu$.count, col = "red")

xsmu2 <- smooth(xsum, 1, type = "regress")
plot(xsmu$x, xsmu$.count, type = "l", xlim = c(0, 50))
lines(xsmu2$x, xsmu2$.count, col = "red")
# Note difference in tails
}

