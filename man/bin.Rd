\name{bin}
\alias{bin}
\title{Create a binned variable.}
\usage{
  bin(x, width = find_width(x),
    origin = find_origin(x, width), name = NULL)
}
\arguments{
  \item{x}{numeric or integer vector}

  \item{width}{bin width}

  \item{origin}{if not specified, guessed by
  \code{\link{find_origin}}}

  \item{name}{name of original variable. This will be guess
  from the input to \code{group} if not supplied. Used in
  the output of \code{\link{condense}} etc.}
}
\description{
  Create a binned variable.
}
\details{
  This function produces an R reference class that wraps
  around a C++ function. Generally, you should just treat
  this as an opaque object with reference semantics, and
  you shouldn't call the methods on it - pass it to
  \code{\link{condense}} and friends.
}
\examples{
x <- runif(1e6)
bin(x)
bin(x, 0.01)
}

