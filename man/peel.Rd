\name{peel}
\alias{peel}
\title{Peel off low density regions of the data.}
\usage{
  peel(x, keep = 0.99, central = FALSE)
}
\arguments{
  \item{x}{condensed summary}

  \item{keep}{(approximate) proportion of data to keep. If
  \code{1}, will remove all cells with counts.}

  \item{central}{if \code{TRUE} peels off regions of lowest
  density only from the outside of the data. In 2d this
  works by progressively peeling off convex hull of the
  data: the current algorithm is quite slow.  If
  \code{FALSE}, just removes the lowest density regions
  wherever they are found. Regions with 0 density are
  removed regardless of location.}
}
\description{
  Keeps specified proportion of data by removing the lowest
  density regions, either anywhere on the plot, or for 2d,
  just around the edges.
}
\details{
  This is useful for visualisation, as an easy way of
  focussing on the regions where the majority of the data
  lies.
}
\examples{
x <- rt(1e5, df = 2)
y <- rt(1e5, df = 2)
xysum <- condense(bin(x, 1 / 10), bin(y, 1 / 10))
plot(xysum$x, xysum$y)

plot(peel(xysum, 0.95, central = TRUE)[1:2])
plot(peel(xysum, 0.90, central = TRUE)[1:2])
plot(peel(xysum, 0.50, central = TRUE)[1:2])
}

