#include <algorithm>
#include <Rcpp.h>
#include "group.hpp"
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

// [[Rcpp::export]]
NumericMatrix smooth2d3(const NumericVector& x, const NumericVector& y, 
               const NumericVector& z, 
               const double x_width, const double x_origin, 
               const double y_width, const double y_origin, 
               const double x_sd, const double y_sd, 
               bool standardise = true) {

  GroupFixed gx(x, x_width, x_origin), gy(y, y_width, y_origin);
  int n = x.size();
  int x_bins = gx.nbins(), y_bins = gy.nbins();

  // std::map<int, double> z_out;
  NumericMatrix z_out(x_bins + 1, y_bins + 1);

  for (int i = 0; i < n; ++i) {
    if (z[i] == 0) continue;

    int x_bin = gx.bin(i), y_bin = gy.bin(i);
    // Find bins within 3 sd
    int x_min = std::max(gx.bin(x[i] - 3 * x_sd), 0),  
        y_min = std::max(gy.bin(y[i] - 3 * y_sd), 0),
        x_max = std::min(gx.bin(x[i] + 3 * x_sd), x_bins),
        y_max = std::min(gx.bin(y[i] + 3 * y_sd), y_bins);
    // Rcout << "[" << x_bin << "," << y_bin << "]: " << 
    //   "[" << x_min << "," << y_min << "] x [" << x_max << "," << y_max << "]\n";

    for (int jx = x_min; jx <= x_max; ++jx) {
      for (int jy = y_min; jy <= y_max; ++jy) {
        // Rcout << "  " << jy * x_bins + jx << " [" << jx << "," << jy << "]\n";
        double x_dist = (x[i] - gx.unbin(jx)) / x_sd;
        double y_dist = (y[i] - gy.unbin(jy)) / y_sd;

        // Computing k takes >80% of the time. Can we cache when input
        // and output grids are aligned? - Yes, by supplying a pre-computed 
        // grid...
        double k = R::dnorm(x_dist, 0, 1, 0) * R::dnorm(y_dist, 0, 1, 0);
        z_out(jx, jy) += k * z[i];
      }      
    }
    // Rcout << "--------------\n";
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

  grid <- seq(0, 4, length = 1001)

  system.time(sm <- smooth2d(df$x, df$y, df$z, grid, grid, 0.2, 0.2, FALSE))
  system.time(sm2 <- smooth2d2(df$x, df$y, df$z, grid, grid, 0.2, 0.2, FALSE))

  grid2 <- seq(0, 4, length = 10001)
  system.time(sm2 <- smooth2d2(df$x, df$y, df$z, grid2, grid2, 0.2, 0.2, FALSE))


  system.time(sm3 <- smooth2d3(df$x, df$y, df$z, 4 / 1000, 0, 4 / 1000, 0, 0.2, 0.2, FALSE))
  sm3df <- as.data.frame(sm3)
  names(sm3df) <- c("x", "y", "z")


  all.equal(sm, sm2)
  sum((sm - sm2) ^ 2)
  image(grid, grid, sm - sm2, useRaster = T)
  
  image(grid, grid, sm, useRaster = T)
  points(df$x[df$z > 0], df$y[df$z > 0], pch = 20)
  image(grid, grid, sm2, useRaster = T)
  points(df$x[df$z > 0], df$y[df$z > 0], pch = 20)

*/
