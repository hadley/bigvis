#include <Rcpp.h>
using namespace Rcpp;

template<typename Group, typename Stat>
NumericVector groupwise(const NumericVector& y, const NumericVector& weight, 
                        const Group& grouper) {
  std::vector<Stat> stat;

  int n = grouper.size();
  for(int i = 0; i < n; ++i) {
    int bin = grouper.bin(i);
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