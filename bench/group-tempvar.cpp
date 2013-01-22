// In a function like 
// 
// unsigned int bin(unsigned int i) const {
//   if (ISNAN(x_[i])) return 0;
//   if (x_[i] < origin_) return 0;
// 
//   return (x_[i] - origin_) / width_ + 1;
// }
// 
// should I create my own temporary double val = x_[i] ?
// 
// It looks like it saves ~0.2 ns per invocation, so probably not worth it for
// performance reasons.

#include <Rcpp.h>
using namespace Rcpp;

class Group1 {
    const Fast<NumericVector> x_;
    double width_;
    double origin_;
  public:
    Group1 (const NumericVector& x, double width, double origin = 0)
       : x_(x), width_(width), origin_(origin) {
    }

    unsigned int bin(unsigned int i) const {
      if (ISNAN(x_[i])) return 0;
      if (x_[i] < origin_) return 0;
      
      return (x_[i] - origin_) / width_ + 1;
    }

    int size() const {
      return x_.size();
    }
};

class Group2 {
    const Fast<NumericVector> x_;
    double width_;
    double origin_;
  public:
    Group2 (const NumericVector& x, double width, double origin = 0)
       : x_(x), width_(width), origin_(origin) {
    }

    unsigned int bin(unsigned int i) const {
      double val = x_[i];
      if (ISNAN(val)) return 0;
      if (val < origin_) return 0;
      
      return (val - origin_) / width_ + 1;
    }

    int size() const {
      return x_.size();
    }
};

template<typename Group>
IntegerVector group_out(const Group& group) {
  int n = group.size();
  IntegerVector out(n);
  for(int i = 0; i < n; ++i) {
    out[i] = group.bin(i);
  }

  return out;
}

// [[Rcpp::export]]
IntegerVector group1(const NumericVector& x, double width, double origin = 0) {
  return group_out(Group1(x, width, origin));
}

// [[Rcpp::export]]
IntegerVector group2(const NumericVector& x, double width, double origin = 0) {
  return group_out(Group2(x, width, origin));
}


/*** R
x <- runif(1e6)
library(microbenchmark)
stopifnot(all.equal(group1(x, 1/1000), group2(x, 1/1000)))

(m <- microbenchmark(
  group1(x, 1/1000),
  group2(x, 1/1000)
))
diff(summary(m)$median) / length(x) * 1e9 / 1e3
*/