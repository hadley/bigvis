\name{standardise}
\alias{standardise}
\title{Standardise a summary to sum to one.}
\usage{
  standardise(x, margin = NULL)
}
\arguments{
  \item{x}{}

  \item{margin}{margins to standardise along.  If
  \code{NULL}, the default, standardises the whole array.}
}
\description{
  Standardise a summary to sum to one.
}
\examples{
b1 <- bin_nd(mtcars, "mpg", 0.01)
plot(b1)
d1 <- density_1d(b1, 0.5)
plot(d1)
# Note change in x-axis limits
plot(standardise(d1))

# Can also standardise a dimension at a time
b2 <- bin_nd(mtcars, c("mpg", "wt"))
plot(b2)
plot(standardise(b2)) # doesn't look any different because no legend
plot(standardise(b2, 1)) # each row sums to 1
plot(standardise(b2, 2)) # each col sums to 1
}

