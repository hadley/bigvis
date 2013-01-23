// Convolution options:
//   zero pad
//   truncuate
//   reflect
//   wrap

#include <Rcpp.h>
using namespace Rcpp;

//' 1d kernel smoothing
//'
//' @param x ordered vector of x positions
//' @param z vector of values
//' @param x_out vector of x positions to smooth for
//' @param kernel function that when given distance between two locations 
//'   returns the weight that should be used.
//' @keywords internal
// [[Rcpp::export]]
NumericVector smooth_1d_(const NumericVector& x, const NumericVector& z, 
                        const NumericVector& x_out, const Function& kernel, 
                        bool reflect = true) {

  int n_in = x.size(), n_out = x_out.size();
  NumericVector z_out(n_out);

  for (int i = 0; i < n_out; i++) {
    for (int j = 0; j < n_in; j++) {
      double dist = x[j] - x_out[i];
      double k = as<NumericVector>(kernel(dist))[0];
      z_out[i] += z[j] * k;

      // Rcout << "dist: " << dist << "\n";
      // Rcout << "k: " << k << "\n";
      // Rcout << "z_out[" << i << "]: " << z_out[i] << "\n";        

      // // Reflections matter if the corresponding value on the other side would
      // // be outside the grid (either to the right or the left)
      // if (reflect) {
      //   // This skips, but it needs to get added somewhere else
      //   if (x_out[i] < x[0] || x_out[i] > x[n_in]) continue;
      // }

    }
  }

  return z_out;
}

/*** R

  x <- as.numeric(1:10)
  z <- rep(c(1, 2), length = length(x))
  k <- kernel("norm", sd = 0.1)
  grid <- seq(0, 11, length = 1000)

  s <- smooth_1d_(x, z, grid, k)
  plot(grid, s, type = "l", ylim = range(s, z))
  points(x, z)

*/