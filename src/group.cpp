#include <Rcpp.h>
#include "group.hpp"
#include "group-hex.hpp"
using namespace Rcpp;

template<typename Group>
IntegerVector group_out(const Group& group) {
  int n = group.size();
  IntegerVector out(n);
  for(int i = 0; i < n; ++i) {
    out[i] = group.bin_i(i);
  }

  return out;
}

RCPP_MODULE(Group) {
  class_<GroupFixed>("GroupFixed")
  .constructor<const NumericVector&, double, double>()
  .const_method("bin_i", &GroupFixed::bin_i)
  .const_method("bin", &GroupFixed::bin)
  .const_method("unbin", &GroupFixed::unbin)

  .const_method("size", &GroupFixed::size)
  .const_method("nbins", &GroupFixed::nbins)

  .const_method("origin", &GroupFixed::origin)
  .const_method("width", &GroupFixed::width)
  ;
}
RCPP_EXPOSED_AS(GroupFixed);
RCPP_EXPOSED_WRAP(GroupFixed);


// [[Rcpp::export]]
IntegerVector group_fixed(const NumericVector& x, double width, double origin = 0) {
  return group_out(GroupFixed(x, width, origin));
}

// [[Rcpp::export]]
IntegerVector group_rect(const NumericVector& x, const NumericVector& y, 
                         double x_width, double y_width,
                         double x_origin, double y_origin) {
  return group_out(Group2d<GroupFixed>(
    GroupFixed(x, x_width, x_origin), 
    GroupFixed(y, y_width, y_origin)));
}


// [[Rcpp::export]]
IntegerVector group_hex(const NumericVector& x, const NumericVector& y, 
                         double x_width, double y_width,
                         double x_origin, double y_origin,
                         double x_max) {
  return group_out(GroupHex(x, y, x_width, y_width, x_origin, y_origin, x_max));
}
