#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector frange(const NumericVector& x, const bool na_rm) {
  NumericVector out(2);
  out[0] = R_PosInf;
  out[1] = R_NegInf;

  int n = x.length();
  for(int i = 0; i < n; ++i) {
    if (!na_rm && R_IsNA(x[i])) {
      out[0] = NA_REAL;
      out[1] = NA_REAL;
      return out;
    }

    if (x[i] < out[0]) out[0] = x[i];
    if (x[i] > out[1]) out[1] = x[i];
  }

  return out;

}
