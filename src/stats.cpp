#include <Rcpp.h>
using namespace Rcpp;

struct Regression {
  double alpha, beta;
};

// [[Rcpp::export]]
double bisquare(double u, double b) {
  u = fabs(u);
  return (u < b) ? pow(1 - pow(u / b, 2), 2) : 0;
}

Regression simpleLinearRegression(const std::vector<double>& x, 
                                  const std::vector<double>& y,
                                  const std::vector<double>& w) {
  int n = x.size();

  double x_wsum = 0, y_wsum = 0, w_sum = 0;
  for (int i = 0; i < n; ++i) {
    x_wsum += x[i] * w[i];
    y_wsum += y[i] * w[i];
    w_sum += w[i];
  };
  double x_mean = x_wsum / w_sum, y_mean = y_wsum / w_sum;

  double var_xy = 0, var_x = 0;
  for (int i = 0; i < n; ++i) {
    var_xy += w[i] * (x[i] - x_mean) * (y[i] - y_mean);
    var_x += w[i] * pow((x[i] - x_mean), 2);
  }

  Regression results;
  results.beta = (var_xy / var_x);
  results.alpha = y_mean - results.beta * x_mean;
  return results;
}

// [[Rcpp::export]]
NumericVector regress(const std::vector<double>& x, 
                      const std::vector<double>& y,
                      const std::vector<double>& w) {
  Regression regression = simpleLinearRegression(x, y, w);
  return NumericVector::create(regression.alpha, regression.beta);
}

// [[Rcpp::export("medianC")]]
double median(const std::vector<double>& x_) {
  if (x_.empty()) return NAN;

  std::vector<double> x(x_);
  int size = x.size();
  std::vector<double>::iterator upper = x.begin() + (int) (size / 2);
  std::nth_element(x.begin(), upper, x.end());

  if (size % 2 == 1) {
    return *upper;
  } else {
    std::vector<double>::iterator lower = upper - 1;
    std::nth_element(x.begin(), lower, upper);
    return (*upper + *lower) / 2.0;
  }  
}

Regression simpleLoess(const std::vector<double>& x, 
                       const std::vector<double>& y,
                       const std::vector<double>& w,
                       int iterations = 3) {
  int n = x.size();
  Regression prev = simpleLinearRegression(x, y, w);

  for (int k = 0; k < iterations; ++k) {
    std::vector<double> resid(n);
    for (int i = 0; i < n; ++i) {
      resid[i] = abs(y[i] - (prev.alpha + prev.beta * x[i]));
    }

    std::vector<double> w_(w);
    double b = 6 * median(resid);
    for (int i = 0; i < n; ++i) {
      w_[i] *= bisquare(resid[i], b);
    }

    prev = simpleLinearRegression(x, y, w_);
  }

  return prev;
}

// [[Rcpp::export]]
NumericVector simple_loess(const std::vector<double>& x, 
                           const std::vector<double>& y,
                           const std::vector<double>& w,
                           int iterations = 3) {
  Regression regression = simpleLoess(x, y, w, iterations);
  return NumericVector::create(regression.alpha, regression.beta);
}
