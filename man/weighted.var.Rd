\name{weighted.var}
\alias{weighted.sd}
\alias{weighted.var}
\title{Compute a weighted variance or standard deviation of a vector.}
\usage{
  weighted.var(x, w = NULL, na.rm = FALSE)

  weighted.sd(x, w, na.rm = TRUE)
}
\arguments{
  \item{x}{numeric vector of observations}

  \item{w}{integer vector of weights, representing the
  number of time each \code{x} was observed}
}
\description{
  Compute a weighted variance or standard deviation of a
  vector.
}
\details{
  Note that unlike the base R \code{\link{var}} function,
  these functions only work with individual vectors not
  matrices or data frames.
}
\examples{
x <- c(1:5)
w <- rpois(5, 5) + 1
y <- x[rep(seq_along(x), w)]
weighted.var(x, w)
var(y)

stopifnot(all.equal(weighted.var(x, w), var(y)))
}
\seealso{
  \code{\link[stats]{weighted.mean}}

  Other weighted statistics: \code{\link{weighted.ecdf}},
  \code{\link{weighted.quantile}}
}

