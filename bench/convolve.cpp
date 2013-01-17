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

// Change order of loops - small improvement but not worth worrying about
// [[Rcpp::export]]
NumericVector convolve3(NumericVector x, NumericVector kernel){
  int n_x = x.size(), n_k = kernel.size();
  NumericVector out(n_x + n_k - 1);

  Fast<NumericVector> fx(x), fkernel(kernel), fout(out);  
  for (int i = 0; i < n_k; i++)
    for (int j = 0; j < n_x; j++) 
      fout[i + j] += fkernel[i] * fx[j];

  return out;
}

// Using references instead of copies makes no impact
// [[Rcpp::export]]
NumericVector convolve4(const NumericVector& x, const NumericVector& kernel){
  int n_x = x.size(), n_k = kernel.size();
  NumericVector out(n_x + n_k - 1);

  Fast<NumericVector> fx(x), fkernel(kernel), fout(out);  
  for (int i = 0; i < n_k; i++)
    for (int j = 0; j < n_x; j++) 
      fout[i + j] += fkernel[i] * fx[j];

  return out;
}

/*** R 
  library(microbenchmark)
  x <- sample(10, 1e5, rep = T)
  kernel <- pnorm(seq(-3, 3, length = 100))
  
  all.equal(convolve1(x, kernel), convolve2(x, kernel))
  all.equal(convolve1(x, kernel), convolve3(x, kernel))

  microbenchmark(
    convolve1(x, kernel),
    convolve2(x, kernel),
    convolve3(x, kernel),
    convolve4(x, kernel)
  )

*/