#include <Rcpp.h>
#include "group.hpp"
using namespace Rcpp;

template<typename Group>
IntegerVector group_out(const Group& group) {
  int n = group.size();
  IntegerVector out(n);
  for(int i = 0; i < n; ++i) {
    out[i] = group.bin(i);
  }

  return out;
}

// [Rcpp::export]
IntegerVector group_fixed(const NumericVector& x, double width, double origin = 0) {
  return group_out(GroupFixed(x, width, origin));
}

// [Rcpp::export]
IntegerVector group_breaks(const NumericVector& x, const NumericVector& breaks) {
  return group_out(GroupBreaks(x, breaks));
}