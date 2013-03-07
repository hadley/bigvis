#include <bigvis.h>
using namespace Rcpp;

RCPP_MODULE(BigVis) {
  class_<BinnedVectorReference>("BinnedVector")
    .constructor<NumericVector, String, double, double>()
    .const_method("bin_i", &BinnedVectorReference::bin_i)
    .const_method("bin", &BinnedVectorReference::bin)
    .const_method("unbin", &BinnedVectorReference::unbin)
    .const_method("nbins", &BinnedVectorReference::nbins)
    .const_method("size", &BinnedVectorReference::size)
    .const_method("origin", &BinnedVectorReference::origin)
    .const_method("width", &BinnedVectorReference::width)
    .const_method("name", &BinnedVectorReference::name)
  ;
  class_<BinnedVectors>("BinnedVectors")
    .constructor<List>()
    .method("add_vector", &BinnedVectors::add_vector)
    .field("bins", &BinnedVectors::bins_)
    .const_method("bin_i", &BinnedVectors::bin_i)
    .const_method("bin", &BinnedVectors::bin)
    .const_method("unbin", &BinnedVectors::unbin)
    .const_method("nbins", &BinnedVectors::nbins)
  ;
}
