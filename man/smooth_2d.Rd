\name{smooth_2d}
\alias{smooth_2d}
\title{Smooth 2d summary statistics}
\usage{
  smooth_2d(summary, vars = NULL, x_bw = NULL, y_bw = NULL,
    x_grid = NULL, y_grid = NULL, x_n = NULL, y_n = NULL,
    standardise = TRUE)
}
\description{
  Smooth 2d summary statistics
}
\examples{
x <- runif(1e3)
y <- runif(1e3)
z <- summarise_2d(x, y, x_binwidth = 1/1000, y_binwidth = 1/1000)
qplot(x, y, fill = count, data = z, geom = "raster")
zs <- smooth_2d(z, x_n = 100, y_n = 100)
qplot(x, y, fill = count, data = zs, geom = "raster")

ggplot(mapping = aes(x, y)) +
  geom_raster(aes(fill = count), data = z) +
  geom_contour(aes(z = count), data = zs, colour = "grey50")
}

