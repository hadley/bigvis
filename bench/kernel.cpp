// Differences in kernel performance

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector normal_kernel(NumericVector x) {
  int n = x.size();
  NumericVector out(n);

  for (int i = 0; i < n; ++i) {
    out[i] = R::dnorm(x[i], 0, 1, 0);
  }

  return out;
}

// [[Rcpp::export]]
double tricube2(double x) {
  x = fabs(x);
  if (x > 1) return 0;

  return pow(1 - pow(x, 3), 3);
}

// [[Rcpp::export]]
double tricube(double x) {
  x = fabs(x);
  if (x > 1) return 0;

  double y = 1 - x * x * x;
  return y * y * y;
}

// [[Rcpp::export]]
NumericVector tricube_kernel(NumericVector x) {
  int n = x.size();
  NumericVector out(n);

  for (int i = 0; i < n; ++i) {
    out[i] = tricube(x[i]);
  }

  return out;
}

// [[Rcpp::export]]
NumericVector copy(NumericVector x) {
  int n = x.size();
  NumericVector out(n);

  for (int i = 0; i < n; ++i) {
    out[i] = x[i];
  }

  return out;
}

/*** R
options(digits = 3)
library(microbenchmark)

x <- runif(1e4)

mean(sapply(x, tricube) - sapply(x, tricube2))

microbenchmark(
  copy(x),
  tricube_kernel(x),
  normal_kernel(x)
)

*/