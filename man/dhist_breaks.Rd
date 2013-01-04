\name{dhist_breaks}
\alias{dhist_breaks}
\title{Diagonal cut histogram.}
\usage{
  dhist_breaks(a = NULL, nbins = 10, rx = NULL)
}
\arguments{
  \item{a}{slope. See paper for details.}

  \item{nbins}{number of bins. See paper for details.}

  \item{rx}{range of data, if not taken from data.}
}
\value{
  A function that takes a single parameter, a numeric x
  specifying the data for which breaks are needed, and
  returns a vector of breaks.
}
\description{
  An another algorithm for computing histogram breaks.
  Produces irregular bins.
}
\examples{
hist(mtcars$mpg, breaks=dhist_breaks())
hist(mtcars$mpg, breaks=dhist_breaks(a=500))
}
\author{
  Lorraine Denby
}
\references{
  L. Denby and C. Mallows. Variations on the histogram.
  Journal of Computational and Graphical Statistics, 18
  (1):21-31, 2009.  URL
  \url{http://pubs.amstat.org/doi/abs/10.1198/jcgs.2009.0002.}
}
\seealso{
  \url{http://pubs.research.avayalabs.com/pdfs/ALR-2007-003-paper.pdf}
}

