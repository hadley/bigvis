\name{bin_nd}
\alias{bin_nd}
\title{Efficient binning for large data.}
\usage{
  bin_nd(df, vars, binwidth = 1L, origin = NULL)
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
  and produces an array of binned counts for the specified
  variables.  It is highly optimised for the fixed bin
  width case.
}
\examples{
bin_1d(mtcars, "mpg", 5)
bin_1d(mtcars, "mpg", 5, 0)
}

