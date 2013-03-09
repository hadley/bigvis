\name{rmse_cvs}
\alias{rmse_cvs}
\title{Estimate smoothing RMSE using leave-one-out cross-valdation.}
\usage{
  rmse_cvs(x, hs = h_grid(x), ...)
}
\description{
  Estimate smoothing RMSE using leave-one-out
  cross-valdation.
}
\examples{
set.seed(1014)
# 1d -----------------------------
x <- rchallenge(1e4)
xsum <- condense(bin(x, 1 / 10))
cvs <- rmse_cvs(xsum)

if (require("ggplot2")) {
autoplot(xsum)
qplot(x, err, data = cvs, geom = "line")
xsmu <- smooth(xsum, 1.3)
autoplot(xsmu)
autoplot(peel(xsmu))
}

# 2d -----------------------------
y <- runif(1e4)
xysum <- condense(bin(x, 1 / 10), bin(y, 1 / 100))
cvs <- rmse_cvs(xysum, h_grid(xysum, 10))
if (require("ggplot2")) {
qplot(x, y, data = cvs, size = err)
}
}

