\name{kernel}
\alias{kernel}
\title{A trimmed kernel function.}
\usage{
  kernel(dist, ..., range = c(0.005, 0.995))
}
\description{
  The resulting kernel will not integrate to 1: the
  assumption is that the mass outside the range is
  negligible. This is done for efficiency reasons.
}

