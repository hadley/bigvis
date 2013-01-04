#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

class BinFixed {
    double width_;
    double origin_;
  public:
    BinFixed (double width, double origin = 0) {
      width_ = width;
      origin_ = origin;
    }
    int inline operator() (double val) const { 
      return (val - origin_) / width_;
    }
};
class BinBreaks {
    NumericVector breaks_;
    NumericVector::iterator breaks_it_, breaks_end_;

  public:
    BinBreaks (NumericVector& breaks) {
      breaks_ = breaks;
      breaks_it_ = breaks.begin();
      breaks_end_ = breaks.end();
    }
    int inline operator() (double val) const { 
      NumericVector::iterator 
        bin_it = std::upper_bound(breaks_it_, breaks_end_, val);

      return std::distance(breaks_it_, bin_it);
    }
};

template<typename Binner>
std::vector<int> bin_1d(NumericVector x, Binner binner) {
  int bin, nmissing = 0;
  std::vector<int> out;

  Fast<NumericVector> fx(x);
  int n = x.size();

  for(int i = 0; i < n; ++i) {
    double val = fx[i];
    if (ISNAN(val)) {
      ++nmissing;
    } else {
      bin = binner(val);
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

// [[Rcpp::export]]
std::vector<int> bin_1d_fixed(NumericVector x, double width, double origin = 0) {
  return bin_1d(x, BinFixed(width, origin));
}

// [[Rcpp::export]]
std::vector<int> bin_1d_breaks(NumericVector x, NumericVector breaks) {
  return bin_1d(x, BinBreaks(breaks));
}

