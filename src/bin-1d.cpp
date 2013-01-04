#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector bin_1d(NumericVector x, NumericVector breaks) {
  // Put missing values in the last position
  int n = breaks.size(), bin;
  IntegerVector out(n + 1);

  NumericVector::iterator x_it = x.begin(), x_end, bin_it,
    breaks_it = breaks.begin(), breaks_end = breaks.end();

  for(; x_it != x.end(); ++x_it) {
    double val = *x_it;
    if (ISNAN(val)) {
      ++out[n];
    } else {
      bin_it = std::upper_bound(breaks_it, breaks_end, val);
      bin = std::distance(breaks_it, bin_it);
      ++out[bin];
    }
  }

  return(out);
}

// [[Rcpp::export]]
std::vector<int> bin_1d_fixed(NumericVector x, double width, double origin) {
  int bin, nmissing = 0;
  std::vector<int> out;

  NumericVector::iterator x_it = x.begin(), x_end;
  for(; x_it != x.end(); ++x_it) {
    double val = *x_it;
    if (ISNAN(val)) {
      ++nmissing;
    } else {
      bin = (val - origin) / width;
      if (bin < 0) continue;
    
      // Make sure there's enough space
      if (bin >= out.size()) {
        out.resize(bin + 1);
      }
      ++out[bin];
    }
  }

  // Put missing values in the last position
  out.push_back(nmissing);
  return out;
}
