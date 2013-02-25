#' The big vis package.
#'
#' @useDynLib bigvis
#' @docType package
#' @name bigvis
NULL

if (!exists("BigVis")) {
  BigVis <- Module("BigVis")
}
