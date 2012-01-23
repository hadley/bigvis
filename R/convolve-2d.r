library(inline)
# 2d convolution -------------------------------------------------------------

convolve_2d <- cxxfunction(signature(sampleS = "numeric", kernelS = "numeric"), plugin = "Rcpp", '
    Rcpp::NumericMatrix sample(sampleS), kernel(kernelS);
    int x_s = sample.nrow(), x_k = kernel.nrow();
    int y_s = sample.ncol(), y_k = kernel.ncol();
    
    Rcpp::NumericMatrix output(x_s + x_k - 1, y_s + y_k - 1);
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
')


