#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix convolve_2d(NumericMatrix sample, NumericMatrix kernel) {
  int x_s = sample.nrow(), x_k = kernel.nrow();
  int y_s = sample.ncol(), y_k = kernel.ncol();
  
  NumericMatrix output(x_s + x_k - 1, y_s + y_k - 1);
  for (int row = 0; row < x_s; row++) {
    for (int col = 0; col < y_s; col++) {
      for (int i = 0; i < x_k; i++) {
        for (int j = 0; j < y_k; j++) {
          output(row + i, col + j) += sample(row, col) * kernel(i, j);
        }
      }
    }
  }
  return output;
}
