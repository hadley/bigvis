#include <algorithm>
#include <Rcpp.h>
#include "group.hpp"
#include "Summary2d.hpp"
#include <memory>
using namespace Rcpp;

std::auto_ptr<Summary2d> createSummary(std::string type) {
  if (type == "mean") {
    return std::auto_ptr<Summary2d>(new Summary2dMean());
  } else if (type == "regression") {
    return std::auto_ptr<Summary2d>(new Summary2dRegression());
  } else if (type == "robust_regression") {
    return std::auto_ptr<Summary2d>(new Summary2dRobustRegression());
  } else {
    stop("Unknown type");
    // Quiet warning
    return std::auto_ptr<Summary2d>(new Summary2dMean());
  }
}

double tricube(double x) {
  if (NumericVector::is_na(x)) return 0;
  x = fabs(x);
  if (x > 1) return 0;

  double y = 1 - x * x * x;
  return y * y * y;
}

bool both_na(double x, double y) {
  return (NumericVector::is_na(x) && NumericVector::is_na(y));
}

// [[Rcpp::export]]
NumericVector smooth_nd_1(const NumericMatrix& grid_in, 
                          const NumericVector& z_in, 
                          const NumericVector& w_in_,
                          const NumericMatrix& grid_out, 
                          const int var, const double h,
                          const std::string type = "mean") {

  if (var < 0) stop("var < 0");
  if (var >= grid_in.ncol()) stop("var too large");
  if (h <= 0) stop("h <= 0");
  if (grid_in.ncol() != grid_out.ncol()) stop("Incompatible grid sizes");

  int n_in = grid_in.nrow(), n_out = grid_out.nrow(), d = grid_in.ncol();
  NumericVector w_in = (w_in_.size() > 0) ? w_in_ : 
    rep(NumericVector::create(1), n_in);
  NumericVector z_out(n_out), w_out(n_out);

  // Will be much more efficient to slice up by input dimension:
  // and most efficient way of doing that will be to bin with / bw
  // My data structure: sparse grids
  // 
  // And once we're smoothing in one direction, with guaranteed e2venly spaced
  // grid can avoid many kernel evaluations and can also compute more
  // efficient running sum

  for(int j = 0; j < n_out; ++j) {
      std::auto_ptr<Summary2d> summary = std::auto_ptr<Summary2d>(createSummary(type));
    for (int i = 0; i < n_in; ++i) {
      // Check that all variables (apart from var) are equal
      bool equiv = true;
      for (int k = 0; k < d; ++k) {
        if (k == var) continue;

        double in = grid_in(i, k), out = grid_out(j, k);
        if (in != out && !both_na(in, out)) {
          equiv = false;
          break;
        }
      };
      if (!equiv) continue;

      double in = grid_in(i, var), out = grid_out(j, var);
      double dist = both_na(in, out) ? 0 : in - out;
      double w = tricube(dist / h) * w_in[i];
      if (w == 0) continue;

      summary->push(dist, z_in[i], w);
    }
    z_out[j] = summary->compute();
  }

  return z_out;
}

// [[Rcpp::export]]
NumericVector smooth_nd(const NumericMatrix& grid_in, 
                        const NumericVector& z_in, 
                        const NumericVector& w_in_,
                        const NumericMatrix& grid_out, 
                        const NumericVector h) {

  if (grid_in.nrow() != z_in.size()) stop("Incompatible input lengths");
  if (grid_in.ncol() != grid_out.ncol()) stop("Incompatible grid sizes");
  if (h.size() != grid_in.ncol()) stop("Incorrect h length");

  int n_in = grid_in.nrow(), n_out = grid_out.nrow(), d = grid_in.ncol();
  NumericVector w_in = (w_in_.size() > 0) ? w_in_ : 
    rep(NumericVector::create(1), n_in);
  NumericVector z_out(n_out), w_out(n_out);

  for (int i = 0; i < n_in; ++i) {
    for(int j = 0; j < n_out; ++j) {
      double w = 1;
      for (int k = 0; k < d; ++k) {
        double dist = (grid_in(i, k) - grid_out(j, k)) / h[k];
        w *= tricube(dist);
      }
      w *= w_in[i];

      w_out[j] += w;
      z_out[j] += z_in[i] * w;
    }
  }

  for(int j = 0; j < n_out; ++j) {
    z_out[j] /= w_out[j];
  }

  return z_out;
}
