#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix smooth2d(const NumericVector& x, const NumericVector& y, 
                        const NumericVector& z, 
                        const NumericVector& x_out, const NumericVector& y_out, 
                        const double x_sd, const double y_sd, 
                        bool standardise = true) {

  if (x.size() != y.size() || x.size() != z.size()) stop("Unequal input lengths");
  if (x_out.size() != y_out.size()) stop("Unequal output lengths");

  int n_in = x.size(), nx_out = x_out.size(), ny_out = y_out.size();
  NumericMatrix z_out(nx_out, ny_out), w_out(nx_out, ny_out);

  for (int i = 0; i < n_in; ++i) {
    for(int jx = 0; jx < nx_out; ++jx) {
      for(int jy = 0; jy < ny_out; ++jy) {
        double x_dist = (x[i] - x_out[jx]) / x_sd;
        double y_dist = (y[i] - y_out[jy]) / y_sd;

        if (fabs(x_dist) > 4 || fabs(y_dist) > 4) continue; 
        double k = R::dnorm(x_dist, 0, 1, 0) * R::dnorm(y_dist, 0, 1, 0);
        z_out(jx, jy) += z[i] * k;
        if (standardise) w_out(jx, jy) += k;
      }
    }
  }

  if (standardise) {
    for(int jx = 0; jx < nx_out; ++jx) {
      for(int jy = 0; jy < ny_out; ++jy) {
        z_out(jx, jy) /= w_out(jx, jy);
      }
    }    
  }

  return z_out;
}

/*** R

  m <- matrix(nrow = 3, byrow = T, c(
    1, 0, 0,
    1, 1, 2,
    0, 0, 1
  ))
  df <- data.frame(x = c(row(m)), y = c(col(m)), z = c(m))
  df <- df[order(df$x, df$y), ]

  grid <- seq(0, 4, by = 0.05)

  sm <- smooth2d(df$x, df$y, df$z, grid, grid, 0.2, 0.2, FALSE)
  
  image(1:3, 1:3, m, xlim = range(grid), ylim = range(grid))
  image(grid, grid, sm)


*/
