#include <Rcpp.h>
#include "stat.cpp"
#include "group.cpp"
#include "group-wise.cpp"
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector compute_sum_breaks(const NumericVector& x, const NumericVector& y, const NumericVector& weight, NumericVector& breaks) {
  return groupwise<GroupBreaks, StatSum>(y, weight, GroupBreaks(x, breaks));
}

// [[Rcpp::export]]
NumericVector compute_sum_fixed(const NumericVector& x, const NumericVector& y, const NumericVector& weight, double width, double origin) {
  return groupwise<GroupFixed, StatSum>(y, weight, GroupFixed(x, width, origin));
}

// [[Rcpp::export]]
NumericVector compute_mean_breaks(const NumericVector& x, const NumericVector& y, const NumericVector& weight, NumericVector& breaks) {
  return groupwise<GroupBreaks, StatMean>(y, weight, GroupBreaks(x, breaks));
}

// [[Rcpp::export]]
NumericVector compute_mean_fixed(const NumericVector& x, const NumericVector& y, const NumericVector& weight, double width, double origin) {
  return groupwise<GroupFixed, StatMean>(y, weight, GroupFixed(x, width, origin));
}

// [[Rcpp::export]]
NumericVector compute_median_breaks(const NumericVector& x, const NumericVector& y, const NumericVector& weight, NumericVector& breaks) {
  return groupwise<GroupBreaks, StatMedian>(y, weight, GroupBreaks(x, breaks));
}

// [[Rcpp::export]]
NumericVector compute_median_fixed(const NumericVector& x, const NumericVector& y, const NumericVector& weight, double width, double origin) {
  return groupwise<GroupFixed, StatMedian>(y, weight, GroupFixed(x, width, origin));
}

// [[Rcpp::export]]
NumericVector compute_sd_breaks(const NumericVector& x, const NumericVector& y, const NumericVector& weight, NumericVector& breaks) {
  return groupwise<GroupBreaks, StatSd>(y, weight, GroupBreaks(x, breaks));
}

// [[Rcpp::export]]
NumericVector compute_sd_fixed(const NumericVector& x, const NumericVector& y, const NumericVector& weight, double width, double origin) {
  return groupwise<GroupFixed, StatSd>(y, weight, GroupFixed(x, width, origin));
}

