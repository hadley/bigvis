#include <Rcpp.h>
#include "summary.hpp"
using namespace Rcpp;

template<typename Summary>
NumericVector summary_compute(const NumericVector& x, Summary summary) {
  int n = x.size();
  for(int i = 0; i < n; ++i) {
    summary.push(x[i], 1);
  }

  int m = summary.size();
  NumericVector out(m);
  for(int i = 0; i < m; ++i) {
    out[i] = summary.compute(i);
  }

  return out;
}

// [[Rcpp::export]]
NumericVector compute_moments(const NumericVector& x) {
  return summary_compute(x, SummaryMoments(2));
}

// [[Rcpp::export]]
NumericVector compute_sum(const NumericVector& x) {
  return summary_compute(x, SummarySum(1));
}

// [[Rcpp::export]]
NumericVector compute_median(const NumericVector& x) {
  return summary_compute(x, SummaryMedian());
}
