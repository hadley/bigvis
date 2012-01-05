library(inline)

# Automatically zero-pads sample so that kernel trails off to zero.
#' @author Dirk Eddelbuettel
#' License GPL-2
convolve_1d <- cxxfunction(signature(sampleS = "numeric", kernelS = "numeric"), plugin = "Rcpp", '
    Rcpp::NumericVector sample(sampleS), kernel(kernelS);
    int n_s = sample.size(), n_k = kernel.size();

    Rcpp::NumericVector output(n_s + n_k - 1, 0.0);
    Rcpp::Range r(0, n_k - 1);
    
    for (int i = 0; i < n_s; i++, r++) {
      output[r] += Rcpp::noNA(sample[i]) * Rcpp::noNA(kernel);
    }
    return output;
')


