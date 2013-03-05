#include <Rcpp.h>
#include "Summary2d.hpp"
using namespace Rcpp;

template<typename Summary>
double summary_2d_compute(const NumericVector& x, const NumericVector& z, 
                                 const NumericVector& w_) {

  Summary summary;
  int n = x.size();
  NumericVector w = (w_.size() > 0) ? w_ : 
    rep(NumericVector::create(1), x.size());

  for(int i = 0; i < n; ++i) {
    summary.push(x[i], z[i], w[i]);
  }
  return summary.compute();
}

// [[Rcpp::export]]
double s2d_kernel_mean(const NumericVector& x, 
  const NumericVector& z, const NumericVector& w) {

  return summary_2d_compute<Summary2dKernelMean>(x, z, w);
}

// [[Rcpp::export]]
double s2d_kernel_regression(const NumericVector& x, 
  const NumericVector& z, const NumericVector& w) {

  return summary_2d_compute<Summary2dKernelRegression>(x, z, w);
}

// [[Rcpp::export]]
double s2d_loess(const NumericVector& x, 
  const NumericVector& z, const NumericVector& w) {

  return summary_2d_compute<Summary2dLoess>(x, z, w);
}

