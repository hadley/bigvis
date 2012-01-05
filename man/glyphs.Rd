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

