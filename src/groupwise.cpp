#include <Rcpp.h>
#include "group.hpp"
#include "stat.hpp"
using namespace Rcpp;

template<typename Group, typename Stat>
NumericVector groupwise(const NumericVector& y, const NumericVector& weight, 
                        const Group& group) {
  std::vector<Stat> stat;

  int n_groups = group.size();
  for(int i = 0; i < n_groups; ++i) {
    int bin = group.bin(i);

    if (bin >= stat.size()) {
      stat.resize(bin + 1);
    }

    stat[bin].push(y[i], weight[i]);
  }

  int n = stat.size();
  int m = stat[0].size();
  NumericMatrix res(n, m);
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      res(i, j) = stat[i].compute(j);
    }
  }
  return res;
}

// -----------------------------------------------------------------------------
// Autogenerated by groupwise-gen.r