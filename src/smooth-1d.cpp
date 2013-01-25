#include <Rcpp.h>
using namespace Rcpp;

//' 1d normal kernel smoothing.
//'
//' This is a variant of \code{\link{density}} for calculating weighted 
//' means or weighted sums, with weights determined by a normal kernel. 
//' Both the input and the output can have irregular locations.
//'
//' @param x ordered vector of x positions
//' @param z vector of values
//' @param x_out vector of x positions to produce smoothed values
//' @param sd standard deviation of normal kernel (the bandwidth of the 
//'   smoother)
//' @param standardise if \code{TRUE}, divides the weighted sum at each location
//'   by the sum of the weights. This is usually what you want for pre-binned
//'   data, as it interpolates between the points, rather than redistributing 
//'   the density.  Any locations in \code{x_out} that are more than 4 standard
//'   deviations away from \code{x_in} will be \code{NaN}.
//' @keywords internal
// [[Rcpp::export]]
NumericVector smooth_1d_normal(const NumericVector& x, const NumericVector& z, 
                        const NumericVector& x_out, const double sd, 
                        bool standardise = true) {

  int n_in = x.size(), n_out = x_out.size();
  NumericVector z_out(n_out), w_out(n_out);

  for (int i = 0; i < n_out; ++i) {
    for (int j = 0; j < n_in; ++j) {
      double dist = x[j] - x_out[i];
      // Only use middle four sd of normal kernel: If we assume that x_out is 
      // sorted, we might be able to do this more efficiently using
      // std::lower_bound and std::upper_bound to find the begin and end 
      // iterators
      if (fabs(dist) > (4 * sd)) continue; 

      double k = R::dnorm(dist, 0.0, sd, 0);
      z_out[i] += z[j] * k;
      if (standardise) w_out[i] += k;
    }
  }

  if (standardise) {
    for (int i = 0; i < n_out; ++i) {
      z_out[i] /= w_out[i];
    }    
  }

  return z_out;
}


/*** R

  x <- as.numeric(1:10)
  z <- rep(c(1, 2), length = length(x))
  k <- kernel("norm", sd = 0.1)
  grid <- seq(0, 11, length = 1000)

  s <- smooth_1d_normal(x, z, grid, 0.1)
  plot(grid, s, type = "l", ylim = range(s, z))
  text(grid, min(s), seq_along(grid) - 1, cex = 0.5)
  text(x, z, seq_along(x) - 1)

  xs <- rep(x, z)
  plot(density(x, 0.1, weights = z))

*/