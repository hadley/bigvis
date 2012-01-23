\name{glyphs}
\alias{glyphs}
\title{Create the data needed to generate a glyph plot.}
\usage{
  glyphs(summary, x_major, x_minor, y_major,
    y_minor = "value", height = rel(0.95),
    width = rel(0.95))
}
\arguments{
  \item{data}{A data frame containing variables named in
  \code{x_major}, \code{x_minor}, \code{y_major} and
  \code{y_minor}.}

  \item{x_major,x_minor,y_major,y_minor}{The name of the
  variable (as a string) for the major and minor x and y
  axes.  Together, each unique \code{y_minor} defaults to
  \code{value} - the value of the summary in each bin.}

  \item{height,width}{The height and width of each glyph.
  Defaults to 95\% of the binwidth of each dimension.
  Specify the width absolutely by supplying a numeric
  vector of length 1, or relative to the binwidth of the
  data by using \code{\link{rel}}.}
}
\description{
  Create the data needed to generate a glyph plot.
}
\examples{
time <- bin_nd(movies, c("year", "length"), c(10, 5))
time <- time[, 1:40]
plot(time)

library(ggplot2)
ratings <- bin_nd(movies, c("year", "length", "rating"), c(10, 5, 0.2),
  c(1900, 0, 1))
ratings <- ratings[, 1:40, ]

ratings_df <- glyphs(ratings, "year", "rating", "length")
qplot(gx, gy, data = ratings_df, geom = "line", group = gid)

ratings_df <- glyphs(standardise(ratings, 1), "year", "rating", "length")
qplot(gx, gy, data = ratings_df, geom = "line", group = gid)

ratings_df <- glyphs(standardise(ratings, 2), "year", "rating", "length")
qplot(gx, gy, data = ratings_df, geom = "line", group = gid)

ratings_df <- glyphs(standardise(ratings, c(1, 2)), "year", "rating", "length")
qplot(gx, gy, data = ratings_df, geom = "line", group = gid)
}

