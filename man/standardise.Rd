\name{standardise}
\alias{standardise}
\title{Standardise a summary to sum to one.}
\usage{
  standardise(x, margin = integer())
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
b1 <- condense(bin(movies$year, 1))
d1 <- smooth(b1, 2, type = "reg")

if (require("ggplot2")) {

autoplot(b1)
autoplot(d1)

# Note change in x-axis limits
autoplot(standardise(d1))
}

# Can also standardise a dimension at a time
b2 <- with(movies, condense(bin(year, 2), bin(length, 10)))
b2 <- peel(b2, central = TRUE)

if (require("ggplot2")) {

autoplot(b2)
autoplot(standardise(b2))    # note legend
autoplot(standardise(b2, "year"))   # each row sums to 1
autoplot(standardise(b2, "length")) # each col sums to 1

base <- ggplot(b2, aes(length, .count)) +
  geom_line(aes(group = year, colour = year))
base
base \%+\% standardise(b2)  # Just affects y axis labels
base \%+\% standardise(b2, "year") # Makes year comparable
base \%+\% standardise(b2, "length") # Meaningless for this display

}
}

