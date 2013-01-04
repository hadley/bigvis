#include <Rcpp.h>
#include <iostream>
#include <algorithm>
using namespace Rcpp;

//' @param breaks must be ordered and span the complete range of x. 
// [[Rcpp::export]]
IntegerVector bin(NumericVector x, NumericVector breaks) {
  // Put missing values in the last position
  int n = breaks.size();
  IntegerVector out(n + 1);

  for(NumericVector::iterator it = x.begin(); it != x.end(); it++) {
    double val = *it;
    if (ISNAN(val)) {
      out[n]++;
    } else {
      NumericVector::iterator bin_it = 
        std::upper_bound(breaks.begin(), breaks.end(), val);

      int bin = std::distance(breaks.begin(), bin_it);
      out[bin]++;
    }
  }

  return out;
}

// [[Rcpp::export]]
IntegerVector bin2(NumericVector x, NumericVector breaks) {
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

  return out;
}

// [[Rcpp::export]]
std::vector<int> bin3(NumericVector x, double width, double origin = 0) {
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

// Create class to encapsulate binning operations ------------------------------


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
std::vector<int> bin_bin(NumericVector x, Binner binner) {
  int bin, nmissing = 0;
  std::vector<int> out;

  NumericVector::iterator x_it = x.begin(), x_end;
  for(; x_it != x.end(); ++x_it) {
    double val = *x_it;
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
std::vector<int> bin_bin_fixed(NumericVector x, double width, double origin = 0) {
  return bin_bin(x, BinFixed(width, origin));
}

// [[Rcpp::export]]
std::vector<int> bin_bin_breaks(NumericVector x, NumericVector breaks) {
  return bin_bin(x, BinBreaks(breaks));
}

// Try using a Fast<NumericVector> ------------------------------
// Barely affects speed
template<typename Binner>
std::vector<int> fbin_bin(NumericVector x, Binner binner) {
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
std::vector<int> fbin_bin_fixed(NumericVector x, double width, double origin = 0) {
  return fbin_bin(x, BinFixed(width, origin));
}

// [[Rcpp::export]]
std::vector<int> fbin_bin_breaks(NumericVector x, NumericVector breaks) {
  return fbin_bin(x, BinBreaks(breaks));
}



/*** R 
library(microbenchmark)
x <- runif(1e5)
breaks <- seq(0, 1, length = 100)

microbenchmark(
  bin(x, breaks),
  bin2(x, breaks),
  bin_bin_breaks(x, breaks),
  fbin_bin_breaks(x, breaks),
  bin3(x, 1/100, 0),
  bin_bin_fixed(x, 1/100, 0),
  fbin_bin_fixed(x, 1/100, 0),
  hist(x, breaks, plot = F)
)

x6 <- runif(1e6)
x7 <- runif(1e7)
x8 <- runif(1e8)

microbenchmark(
  bin3(x6, 1/100, 0),
  bin_bin_fixed(x6, 1/100, 0),
  bin3(x7, 1/100, 0),
  bin_bin_fixed(x7, 1/100, 0),
  bin3(x8, 1/100, 0),
  bin_bin_fixed(x7, 1/100, 0),
  times = 5)

*/