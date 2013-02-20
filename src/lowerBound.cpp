#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

// Quick and dirty implementation of lowerBound, the complement to R's
// findInterval
// [[Rcpp::export]]
IntegerVector lowerBound(const NumericVector& x, const NumericVector& breaks) {
  int n = x.size();
  IntegerVector out(n);

  for (int i = 0; i < n; i++) {
    NumericVector::iterator it =
      std::lower_bound(breaks.begin(), breaks.end(), x[i]);
    if (it == breaks.end()) --it;
    out[i] = it - breaks.begin() + 1;
  }
  return out;
}