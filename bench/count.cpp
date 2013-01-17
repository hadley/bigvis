// Experiment with making the binner more generic, so that the binner
// class also stores the variable being binned over - this is important
// for separating the grouping from the numeric operation.

#include <Rcpp.h>
#include <iostream>
#include <algorithm>
using namespace Rcpp;

template<typename Binner>
std::vector<int> count_x(const NumericVector& x, Binner binner) {
  std::vector<int> out;

  int n = x.size();

  for(int i = 0; i < n; ++i) {
    int bin = binner(x[i]);
    if (bin < 0) continue;
  
    // Make sure there's enough space
    if (bin >= out.size()) {
      out.resize(bin + 1);
    }
    ++out[bin];
  }

  return out;
}

template<typename Binner>
std::vector<int> count(Binner binner) {
  std::vector<int> out;

  int n = binner.size();
  for(int i = 0; i < n; ++i) {
    int bin = binner.bin(i);
    if (bin < 0) continue;
  
    // Make sure there's enough space
    if (bin >= out.size()) {
      out.resize(bin + 1);
    }
    ++out[bin];
  }

  return out;
}


class BinFixed {
    double width_;
    double origin_;
  public:
    BinFixed (double width, double origin = 0) {
      width_ = width;
      origin_ = origin;
    }

    int inline operator() (double val) const { 
      if (ISNAN(val)) return 0;

      return (val - origin_) / width_ + 1;
    }
};

class BinFixed2 {
    const NumericVector& x_;
    double width_;
    double origin_;
  public:
    BinFixed2 (const NumericVector& x, double width, double origin = 0)
       : x_(x), width_(width), origin_(origin) {
    }

    int bin(int i) const { 
      if (ISNAN(x_[i])) return 0;
      return (x_[i] - origin_) / width_ + 1;
    }

    int size() const {
      return x_.size();
    }
};


// [[Rcpp::export]]
std::vector<int> count_x2(NumericVector x, double width, double origin = 0) {
  return count_x(x, BinFixed(width, origin));
}

// [[Rcpp::export]]
std::vector<int> count2(NumericVector x, double width, double origin = 0) {
  return count(BinFixed2(x, width, origin));
}


/*** R 
options(digits = 3)
library(microbenchmark)
x <- runif(1e5)

# Breaks
microbenchmark(
  count_x2(x, 1/100),  
  count2(x, 1/100)
)

*/