#include <bigvis.h>
using namespace Rcpp;

NumericVector frange(const NumericVector& x, const bool finite = true);

int BinnedVector::nbins() const {
  double max = frange(x_)[1];
  return bin(max) + 1; 
  // +1 bin for missing values
}
