\name{peel}
\alias{peel}
\title{Peel off low density regions of the data}
\usage{
  peel(x, keep = 0.99, central = FALSE)
}
\arguments{
  \item{x}{condensed summary}

  \item{keep}{(approximate) amount of data to keep.}

  \item{central}{if \code{TRUE} peels off regions of lowest
  density only from the outside of the data. In 2d this
  works by progressively peeling off convex hull of the
  data. If \code{FALSE}, just removes the lowest density
  regions wherever they are found.}
}
\description{
  Peel off low density regions of the data
}
\examples{
x <- rt(1e5, df = 2)
y <- rt(1e5, df = 2)
xysum <- condense(list(bin(x, 1 / 10), bin(y, 1 / 10)))
plot(xysum$x, xysum$y)

plot(peel(xysum, 0.95, central = TRUE)[1:2])
plot(peel(xysum, 0.90, central = TRUE)[1:2])
plot(peel(xysum, 0.50, central = TRUE)[1:2])
}

