\name{bin_1d}
\alias{bin_1d}
\title{Efficient 1d binning for large data.}
\usage{
  bin_1d(df, x, binwidth, origin = NULL)
}
\arguments{
  \item{df}{Either a data frame, or the name of a xdf
  file.}

  \item{x}{Index of variable of interest (either name or
  numeric position)}

  \item{binwidth}{Width of bin.}

  \item{origin}{Location to start first bin.  If missing,
  defaults to minimum of x variable.}
}
\value{
  A list with components \itemize{ \item \code{count}: an
  integer vector of counts. Will be of length \code{nbin +
  1}: the final bin is the number of missing values.  \item
  \code{range}: a numeric vector giving the range of the
  data \item \code{nbin}: a single integer giving the
  number of bins }
}
\description{
  This function takes a data frame or xdf file as an input
  and produces a vector of bin counts for the specified
  variable.  It is highly optimised for the fixed bin width
  case.
}
\examples{
bin_1d(mtcars, "mpg", 5)
bin_1d(mtcars, "mpg", 5, 0)
}

