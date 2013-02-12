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

        if (fabs(x_dist) > 3 || fabs(y_dist) > 3) continue; 
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

// [[Rcpp::export]]
NumericMatrix smooth2d2(const NumericVector& x, const NumericVector& y, 
                        const NumericVector& z, 
                        const NumericVector& x_out, const NumericVector& y_out, 
                        const double x_sd, const double y_sd, 
                        bool standardise = true) {

  if (x.size() != y.size() || x.size() != z.size()) stop("Unequal input lengths");
  if (x_out.size() != y_out.size()) stop("Unequal output lengths");

  int n_in = x.size(), nx_out = x_out.size(), ny_out = y_out.size();
  NumericMatrix z_out(nx_out, ny_out), w_out(nx_out, ny_out);

  // For each input position, loop through the x and y output positions
  // that are within 3 standard deviations
  NumericVector::iterator x_out_begin = x_out.begin(), x_out_end = x_out.end(),
    y_out_begin = y_out.begin(), y_out_end = y_out.end();

  for (int i = 0; i < n_in; ++i) {
    NumericVector::iterator 
      x_low =  std::lower_bound(x_out_begin, x_out_end, x[i] - 3 * x_sd),
      y_low =  std::lower_bound(y_out_begin, y_out_end, y[i] - 3 * y_sd),
      x_high = std::upper_bound(x_low,       x_out_end, x[i] + 3 * x_sd),
      y_high = std::upper_bound(y_low,       y_out_end, y[i] + 3 * y_sd);

    for(NumericVector::iterator x_it(x_low); x_it != x_high; ++x_it) {
      for(NumericVector::iterator y_it(y_low); y_it != y_high; ++y_it) {
        // Calculate distance
        double x_dist = (x[i] - *x_it) / x_sd;
        double y_dist = (y[i] - *y_it) / y_sd;

        double k = R::dnorm(x_dist, *x_it, x_sd, 0) * R::dnorm(y_dist, 0, 1, 0);
        if (k < 1e-4) continue; 

        // Save in appropriate place in output matrix
        int jx = x_it - x_out.begin(),
            jy = y_it - y_out.begin();
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

  grid <- seq(0, 4, length = 1000)

  system.time(sm <- smooth2d(df$x, df$y, df$z, grid, grid, 0.2, 0.2, FALSE))
  system.time(sm2 <- smooth2d2(df$x, df$y, df$z, grid, grid, 0.2, 0.2, FALSE))

  all.equal(sm, sm2)
  sum((sm - sm2) ^ 2)
  image(grid, grid, sm - sm2, useRaster = T)
  
  image(grid, grid, sm, useRaster = T)
  points(df$x[df$z > 0], df$y[df$z > 0], pch = 20)
  image(grid, grid, sm2, useRaster = T)
  points(df$x[df$z > 0], df$y[df$z > 0], pch = 20)

*/
