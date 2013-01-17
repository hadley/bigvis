// Instead of counting, compute a more complicated statistic: a median

#include <Rcpp.h>
#include <iostream>
#include <algorithm>
using namespace Rcpp;

class BinFixed {
    const Fast<NumericVector> x_;
    double width_;
    double origin_;
  public:
    BinFixed (const NumericVector& x, double width, double origin = 0)
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
class StatMedian {
    std::vector<double> xs;

  public:
    void push(double x) {
      xs.push_back(x);
    }

    double compute() {
      if (xs.empty()) return NAN;
      if (xs.size() % 2 == 0) {
        int mid = xs.size() / 2;
        std::nth_element(xs.begin(), xs.begin() + mid, xs.end());
        double upper = xs[mid];
        std::nth_element(xs.begin(), xs.begin() + mid - 1, xs.end());
        double lower = xs[mid - 1];

        return (lower + upper) / 2;
      } else {
        int mid = xs.size() / 2;
        std::nth_element(xs.begin(), xs.begin() + mid, xs.end());
        return xs[mid];
      }

    }
};

template<typename Binner>
NumericVector group_median(NumericVector& y, Binner binner) {
  std::vector<StatMedian> stat;

  int n = binner.size();
  for(int i = 0; i < n; ++i) {
    int bin = binner.bin(i);
    if (bin < 0) continue;

    if (bin >= stat.size()) {
      stat.resize(bin + 1);
    }

    stat[bin].push(y[i]);
  }

  int m = stat.size();
  NumericVector res(m);
  for (int i = 0; i < m; ++i) {
    res[i] = stat[i].compute();
  }
  return res;
}


// [[Rcpp::export]]
NumericVector group_median_(NumericVector x, NumericVector y,
                            double width, double origin = 0) {
  return group_median(y, BinFixed(x, width, origin));
}


/*** R
options(digits = 3)
library(microbenchmark)
x <- runif(1e5)
y <- runif(1e5)

group_median_tapply <- function(x, y, width, origin = 0) {
  bins <- trunc((x - origin) / width)
  c(NaN, unname(tapply(y, bins, median)))
}
med1 <- group_median_tapply(x, y, width = 1/1000)
med2 <- group_median_(x, y, width = 1/1000)
stopifnot(all.equal(med1, med2))

# Breaks
microbenchmark(
# group_median_tapply(x, y, width = 1/1000),
  group_median_(x, y, width = 1/1000)
)

*/
