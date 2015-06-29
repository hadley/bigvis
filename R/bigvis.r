#' The big vis package.
#'
#' @useDynLib bigvis
#' @docType package
#' @name bigvis
NULL

if (!exists("BigVis")) {
  BigVis <- Rcpp::Module("BigVis")
}


#' @param x,object,... Generic args
#' @rdname bigvis
#' @export
setMethod("show", "Rcpp_BinnedVector", function(object) {
  cat("Binned [", object$size(), "]. ",
    "Width: ", object$width(), " Origin: ", object$origin(), "\n", sep = "")
})

#' @rdname bigvis
#' @export
setMethod("as.integer", "Rcpp_BinnedVector", function(x, ...) {
  vapply(seq_len(x$size()), x$bin_i, integer(1))
})



# Silence R CMD check note
#' @importFrom methods new
#' @importFrom Rcpp compileAttributes cpp_object_initializer
NULL

