#include <Rcpp.h>
using namespace Rcpp;

template<typename Binner, typename Stat>
NumericVector groupwise(NumericVector& y, NumericVector& weight, Binner binner) {
  std::vector<Stat> stat;

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