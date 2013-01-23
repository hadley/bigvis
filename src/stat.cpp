#include <Rcpp.h>
#include "stat.hpp"
using namespace Rcpp;

template<typename Stat>
NumericVector stat_compute(const NumericVector& x, Stat stat) {
  int n = x.size();
  for(int i = 0; i < n; ++i) {
    stat.push(x[i], 1);
  }

  int m = stat.size();
  NumericVector out(m);
  for(int i = 0; i < m; ++i) {
    out[i] = stat.compute(i);
  }

  return out;
}

// [[Rcpp::export]]
NumericVector compute_moments(const NumericVector& x) {
  return stat_compute(x, StatMoments(2));
}

// [[Rcpp::export]]
NumericVector compute_sum(const NumericVector& x) {
  return stat_compute(x, StatSum(1));
}

// [[Rcpp::export]]
NumericVector compute_median(const NumericVector& x) {
  return stat_compute(x, StatMedian());
}