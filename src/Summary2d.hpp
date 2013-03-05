#include <Rcpp.h>
#include "stats.hpp"
using namespace Rcpp;

class Summary2dKernelMean {
    double w_, z_;

  public:
    Summary2dKernelMean() : w_(0), z_(0) {}

    void push(double x, double z, double w) {
      double k = R::dnorm(x, 0, 1, 0) * w;
      w_ += k;
      z_ += z;
    }

    double compute() {
      return z_ / w_;
    }
};

class Summary2dKernelRegression {
    std::vector<double> x_, z_, w_;

  public:
    Summary2dKernelRegression() {}

    void push(double x, double z, double w) {
      x_.push_back(x);
      z_.push_back(z);
      w_.push_back(w);
    }

    double compute() {
      return simpleLinearRegression(x_, z_, w_).alpha;
    }
};

class Summary2dLoess {
    int iterations_;
    std::vector<double> x_, z_, w_;

  public:
    Summary2dLoess() : iterations_(3) {}
    Summary2dLoess(int iterations) : iterations_(iterations) {}

    void push(double x, double z, double w) {
      x_.push_back(x);
      z_.push_back(z);
      w_.push_back(w);
    }

    double compute() {
      return simpleLoess(x_, z_, w_, iterations_).alpha;
    }
};
