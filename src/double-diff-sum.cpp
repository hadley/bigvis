#include <Rcpp.h>
#include <iostream>
#include <algorithm>
using namespace Rcpp;

// Efficiently compute \sum \sum abs(x_i - x_j) for binned data
// 
// [[Rcpp::export]]
std::vector<int> double_diff_sum(IntegerVector bin, IntegerVector count) {
  int n = bin.size();
  std::vector<int> out;

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      int pos = abs(bin[i] - bin[j]);

      if (pos + 1 > out.size()) {
        out.resize(pos + 1);
      }
      out[pos] += count[i] * count[j];
    }
  }

  return out;
}
