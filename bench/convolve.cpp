#include <Rcpp.h>
#include <iostream>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector convolve1(NumericVector x, NumericVector kernel) {
  int n_x = x.size(), n_k = kernel.size();

  NumericVector output(n_x + n_k - 1);
  Range r(0, n_k - 1);
  
  for (int i = 0; i < n_x; i++, r++) {
    output[r] += noNA(x[i]) * noNA(kernel);
  }
  return output;
}

// [[Rcpp::export]]
NumericVector convolve2(NumericVector x, NumericVector kernel){
  int n_x = x.size(), n_k = kernel.size();
  NumericVector out(n_x + n_k - 1);

  Fast<NumericVector> fx(x), fkernel(kernel), fout(out);  
  for (int i = 0; i < n_x; i++)
    for (int j = 0; j < n_k; j++) 
      fout[i + j] += fx[i] * fkernel[j];

  return out;
}

/*** R 
  library(microbenchmark)
  x <- sample(10, 1e4, rep = T)
  kernel <- pnorm(seq(-3, 3, length = 100))

  microbenchmark(
    convolve1(x, kernel),
    convolve2(x, kernel)
  )

*/