\name{best_h}
\alias{best_h}
\title{Find "best" smoothing parameter using leave-one-out cross validation.}
\usage{
  best_h(x, h_init = NULL, ..., tol = 0.01,
    control = list())
}
\arguments{
  \item{x}{condensed summary to smooth}

  \item{h}{initial values of bandwidths to start search
  out. If not specified defaults to 5 times the binwidth of
  each variable.}

  \item{...}{other arguments (like \code{var}) passed on to
  \code{\link{rmse_cv}}}

  \item{tol}{numerical tolerance, defaults to 1\%.}

  \item{control}{additional control parameters passed on to
  \code{\link{optim}} The most useful argument is probably
  trace, which makes it possible to follow the progress of
  the optimisation.}
}
\value{
  a single numeric value representing the bandwidth that
  minimises the leave-one-out estimate of rmse. Vector has
  attributes \code{evaluations} giving the number of times
  the objective function was evaluated. If the optimisation
  does not converge, or smoothing is not needed (i.e. the
  estimate is on the lower bounds), a warning is thrown.
}
\description{
  Minimises the leave-one-out estimate of root mean-squared
  error to find find the "optimal" bandwidth for smoothing.
}
\details{
  L-BFGS-B optimisation is used to constrain the bandwidths
  to be greater than the binwidths: if the bandwidth is
  smaller than the binwidth it's impossible to compute the
  rmse because no smoothing occurs. The tolerance is set
  relatively high for numerical optimisation since the
  precise choice of bandwidth makes little difference
  visually, and we're unlikely to have sufficient data to
  make a statistically significant choice anyway.
}
\examples{
x <- rchallenge(1e4)
xsum <- condense(bin(x, 1 / 10))
h <- best_h(xsum, control = list(trace = 1, REPORT = 1))
h <- best_h(xsum)

if (require("ggplot2")) {
autoplot(xsum)
autoplot(smooth(xsum, h))
}
}
\seealso{
  Other bandwidth estimation functions:
  \code{\link{h_grid}}, \code{\link{rmse_cv}},
  \code{\link{rmse_cvs}}
}

