// Explore opportunities for making smooth_1d faster
// 
// Bounding to a given range is really important, and memoisation helps offset
// the cost of making a call back to R, but the biggest win is using a pure
// C/C++ kernel function.
// 
// 
#include <Rcpp.h>
using namespace Rcpp;

// Base implementation
// [[Rcpp::export]]
NumericVector smooth_1d(const NumericVector& x, const NumericVector& z, 
                        const NumericVector& x_out, const Function& kernel) {

  int n_in = x.size(), n_out = x_out.size();
  NumericVector z_out(n_out);

  for (int i = 0; i < n_out; i++) {
    for (int j = 0; j < n_in; j++) {
      double dist = x[j] - x_out[i];
      double k = as<NumericVector>(kernel(dist))[0];
      z_out[i] += z[j] * k;
    }
  }

  return z_out;
}

// Memoise distance calculations
// [[Rcpp::export]]
NumericVector smooth_1d_memo(const NumericVector& x, const NumericVector& z, 
                        const NumericVector& x_out, const Function& kernel) {
  int n_in = x.size(), n_out = x_out.size();
  NumericVector z_out(n_out);

  std::unordered_map<double, double> k_memo;

  for (int i = 0; i < n_out; i++) {
    for (int j = 0; j < n_in; j++) {
      double dist = x[j] - x_out[i];

      std::unordered_map<double, double>::const_iterator it = k_memo.find(dist);
      double k;
      if (it == k_memo.end()) {
        k = as<NumericVector>(kernel(dist))[0];
        k_memo[dist] = k;
      } else {
        k = it->second; 
      }

      z_out[i] += z[j] * k;
    }
  }

  return z_out;
}

// Use range of kernel
// [[Rcpp::export]]
NumericVector smooth_1d_range(const NumericVector& x, const NumericVector& z, 
                        const NumericVector& x_out, const Function& kernel,
                        double kmin, double kmax) {

  int n_in = x.size(), n_out = x_out.size();
  NumericVector z_out(n_out);

  for (int i = 0; i < n_out; i++) {
    for (int j = 0; j < n_in; j++) {
      double dist = x[j] - x_out[i];
      if (dist < kmin || dist > kmax) continue;

      double k = as<NumericVector>(kernel(dist))[0];
      z_out[i] += z[j] * k;
    }
  }

  return z_out;
}


// Memoise and use range
// [[Rcpp::export]]
NumericVector smooth_1d_memo_range(const NumericVector& x, const NumericVector& z, 
                        const NumericVector& x_out, const Function& kernel,
                        double kmin, double kmax) {
  int n_in = x.size(), n_out = x_out.size();
  NumericVector z_out(n_out);

  std::unordered_map<double, double> k_memo;

  for (int i = 0; i < n_out; i++) {
    for (int j = 0; j < n_in; j++) {
      double dist = x[j] - x_out[i];
      if (dist < kmin || dist > kmax) continue;

      std::unordered_map<double, double>::const_iterator it = k_memo.find(dist);
      double k;
      if (it == k_memo.end()) {
        k = as<NumericVector>(kernel(dist))[0];
        k_memo[dist] = k;
      } else {
        k = it->second; 
      }

      z_out[i] += z[j] * k;
    }
  }

  return z_out;
}

// Memoise and use range
// [[Rcpp::export]]
NumericVector smooth_1d_memo_range_map(const NumericVector& x, const NumericVector& z, 
                        const NumericVector& x_out, const Function& kernel,
                        double kmin, double kmax) {
  int n_in = x.size(), n_out = x_out.size();
  NumericVector z_out(n_out);

  std::map<double, double> k_memo;

  for (int i = 0; i < n_out; i++) {
    for (int j = 0; j < n_in; j++) {
      double dist = x[j] - x_out[i];
      if (dist < kmin || dist > kmax) continue;

      std::map<double, double>::const_iterator it = k_memo.find(dist);
      double k;
      if (it == k_memo.end()) {
        k = as<NumericVector>(kernel(dist))[0];
        k_memo[dist] = k;
      } else {
        k = it->second; 
      }

      z_out[i] += z[j] * k;
    }
  }

  return z_out;
}

// Memoise, use range & use C++ function for kernel
// [[Rcpp::export]]
NumericVector smooth_1d_memo_range_kcpp(const NumericVector& x, const NumericVector& z, 
                        const NumericVector& x_out, double kmin, double kmax) {
  int n_in = x.size(), n_out = x_out.size();
  NumericVector z_out(n_out);

  std::unordered_map<double, double> k_memo;

  for (int i = 0; i < n_out; i++) {
    for (int j = 0; j < n_in; j++) {
      double dist = x[j] - x_out[i];
      if (dist < kmin || dist > kmax) continue;

      std::unordered_map<double, double>::const_iterator it = k_memo.find(dist);
      double k;
      if (it == k_memo.end()) {
        k = Rf_pnorm5(dist, 0.0, 0.1, 1, 0);
        k_memo[dist] = k;
      } else {
        k = it->second; 
      }

      z_out[i] += z[j] * k;
    }
  }

  return z_out;
}

// Use cpp kernel function without memoisation
// [[Rcpp::export]]
NumericVector smooth_1d_range_kcpp(const NumericVector& x, const NumericVector& z, 
                        const NumericVector& x_out, double kmin, double kmax) {

  int n_in = x.size(), n_out = x_out.size();
  NumericVector z_out(n_out);

  for (int i = 0; i < n_out; i++) {
    for (int j = 0; j < n_in; j++) {
      double dist = x[j] - x_out[i];
      if (dist < kmin || dist > kmax) continue;

      double k = Rf_pnorm5(dist, 0.0, 0.1, 1, 0);
      z_out[i] += z[j] * k;
    }
  }

  return z_out;
}


/*** R
  options(digits = 2)
  x <- 1:10
  z <- rep(c(1, 2), length = length(x))
  k <- kernel("norm", sd = 0.1)
  krng <- range(k)
  grid <- seq(0, 11, length = 100)
  
  stopifnot(all.equal(
    smooth_1d_memo_range(x, z, grid, k, krng[1], krng[2]),
    smooth_1d_range_kcpp(x, z, grid, krng[1], krng[2])
  ))

  library(microbenchmark)
  microbenchmark(
    base = smooth_1d(x, z, grid, k),
    memo = smooth_1d_memo(x, z, grid, k),
    range = smooth_1d_range(x, z, grid, k, krng[1], krng[2]),
    "range + kcpp" = smooth_1d_range_kcpp(x, z, grid, krng[1], krng[2]),
    "range + memo" = smooth_1d_memo_range(x, z, grid, k, krng[1], krng[2]),
    "range + memo + kcpp" = smooth_1d_memo_range_kcpp(x, z, grid, krng[1], krng[2]),
    "range + memo + map" = smooth_1d_memo_range_map(x, z, grid, k, krng[1], krng[2])
  )
  
  # More realistic sample sizes
  x <- 1:3e3
  z <- rep(c(1, 2), length = length(x))
  
  grid3 <- seq(0, 11, length = 3e3)
  grid4 <- seq(0, 11, length = 3e4)

  microbenchmark(
    grid3_c = smooth_1d_range_kcpp(x, z, grid3, krng[1], krng[2]),
    grid3_r = smooth_1d_memo_range_map(x, z, grid3, k, krng[1], krng[2]),
    grid4_c = smooth_1d_range_kcpp(x, z, grid4, krng[1], krng[2]),
    grid4_r = smooth_1d_memo_range_map(x, z, grid4, k, krng[1], krng[2]),
    times = 10)

*/