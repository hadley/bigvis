// Instead of counting, compute a more complicated statistic: a weighted mean

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

template<typename Binner>
NumericVector group_mean(NumericVector& y, NumericVector& weight, Binner binner) {
  std::vector<double> count;
  std::vector<double> sum;

  int n = binner.size();
  for(int i = 0; i < n; ++i) {
    int bin = binner.bin(i);
    if (bin < 0) continue;
  
    // Make sure there's enough space
    if (bin >= sum.size()) {
      sum.resize(bin + 1);
      count.resize(bin + 1);
    }

    count[bin] += weight[i];
    sum[bin] += y[i];
  }

  int m = count.size();
  NumericVector res(m);
  for (int i = 0; i < m; ++i) {
    res[i] = sum[i] / count[i];
  }
  return res;
}


class StatMean {
    double count;
    double sum;

  public:
    StatMean () : count(0), sum(0) {
    }
    void push(double x, double weight) {
      count += weight;
      sum += x;
    }

    double compute() {
      return sum / count;
    }
};

template<typename Binner>
NumericVector group_mean2(NumericVector& y, NumericVector& weight, Binner binner) {
  std::vector<StatMean> stat;

  int n = binner.size();
  for(int i = 0; i < n; ++i) {
    int bin = binner.bin(i);
    if (bin < 0) continue;
  
    if (bin >= stat.size()) {
      stat.resize(bin + 1);
    }

    stat[bin].push(y[i], weight[i]);
  }

  int m = stat.size();
  NumericVector res(m);
  for (int i = 0; i < m; ++i) {
    res[i] = stat[i].compute();
  }
  return res;
}


// [[Rcpp::export]]
NumericVector group_mean_(NumericVector x, NumericVector y, NumericVector weight, 
                       double width, double origin = 0) {
  return group_mean(y, weight, BinFixed(x, width, origin));
}
// [[Rcpp::export]]
NumericVector group_mean2_(NumericVector x, NumericVector y, NumericVector weight, 
                       double width, double origin = 0) {
  return group_mean2(y, weight, BinFixed(x, width, origin));
}


/*** R 
options(digits = 3)
library(microbenchmark)
x <- runif(1e6)
y <- runif(1e6)
weight <- rep(1, 1e6)

# Breaks
microbenchmark(
  group_mean_(x, y, weight, width = 1/100),
  group_mean2_(x, y, weight, width = 1/100)
)

*/