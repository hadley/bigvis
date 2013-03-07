#include <Rcpp.h>
#include "stats.hpp"
using namespace Rcpp;

class Summary2d {
  public:
    virtual void push(double x, double z, double w) =0;
    virtual double compute() =0;
    // virtual ~Summary2d();
};

class Summary2dMean: public Summary2d {
    double w_, z_;

  public:
    Summary2dMean() : w_(0), z_(0) {}

    void push(double x, double z, double w) {
      w_ += w;
      z_ += z;
    }

    double compute() {
      return z_ / w_;
    }
};

class Summary2dRegression: public Summary2d {
    std::vector<double> x_, z_, w_;

  public:
    Summary2dRegression() {}

    void push(double x, double z, double w) {
      x_.push_back(x);
      z_.push_back(z);
      w_.push_back(w);
    }

    double compute() {
      return simpleLinearRegression(x_, z_, w_).alpha;
    }
};

class Summary2dRobustRegression: public Summary2d {
    int iterations_;
    std::vector<double> x_, z_, w_;

  public:
    Summary2dRobustRegression() : iterations_(3) {}
    Summary2dRobustRegression(int iterations) : iterations_(iterations) {}

    void push(double x, double z, double w) {
      x_.push_back(x);
      z_.push_back(z);
      w_.push_back(w);
    }

    double compute() {
      return simpleRobustRegression(x_, z_, w_, iterations_).alpha;
    }
};
