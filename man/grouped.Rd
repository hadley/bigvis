\name{grouped}
\alias{grouped}
\title{Create a grouped variable.}
\usage{
  grouped(x, width, origin = NULL)
}
\arguments{
  \item{x}{numeric or integer vector}

  \item{width}{bin width}

  \item{origin}{if not specified, guessed by
  \code{\link{find_origin}}}
}
\description{
  Create a grouped variable.
}
\details{
  This function produces an R reference class that wraps
  around a C++ function. Generally, you should just treat
  this as an opaque object with reference semantics, and
  you shouldn't call the methods on it - pass it to
  \code{\link{summarise_1d}} and friends.
}
\examples{
x <- runif(1e6)
g <- grouped(x, 0.01)
}

