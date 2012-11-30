#' Pick breaks for interval (1d) bins.
#'
#' Specify either \code{bins} or \code{binwidth}.
#'
#' @param bins Desired number of bins
#' @param binwidth Desired bin width
#' @param origin Desired origin of first bin
#' @param range Range of values to use, if different to range of data.
#' @return A function that takes a single parameter, a numeric x specifying
#'   the data for which breaks are needed, and returns a vector of breaks.
#' @export
interval_breaks <- function(bins = 20, binwidth = NULL, origin = NULL, range = NULL) {
  
  function(x) {
    # If x is an integer, place breaks between values
    if (is.integer(x)) return(seq.int(min(x) - 1, max(x), 1) + 0.5)

    if (is.null(range)) {
      range <- range(x, na.rm = TRUE, finite = TRUE)
    }
    # If x is a point mass, make a single bin
    if (diff(range) < 1e-07) return(range)

    if (is.null(binwidth)) {
      binwidth <- diff(range) / bins
      message("Using binwidth ", format(binwidth, digits = 3))
    }

    if (is.null(origin)) {
      breaks <- fullseq(range, binwidth)
    } else {
      breaks <- seq(origin, max(range) + binwidth, binwidth)
    }
    
    breaks
  } 
}

#' Pick breaks automatically using Sturges' rule
#'
#' @export
#' @return A function that takes a single parameter, a numeric x specifying
#'   the data for which breaks are needed, and returns a vector of breaks.
sturges_breaks <- function() {
  function(x) interval_breaks(nclass.Sturges(x))(x)
}

#' Pick breaks automatically using Scott's rule
#'
#' @export
#' @return A function that takes a single parameter, a numeric x specifying
#'   the data for which breaks are needed, and returns a vector of breaks.
scott_breaks <- function() {
  function(x) interval_breaks(nclass.scott(x))(x)
}

#' Pick breaks automatically using the Freedman-Diaconis rule
#'
#' @export
#' @return A function that takes a single parameter, a numeric x specifying
#'   the data for which breaks are needed, and returns a vector of breaks.
fd_breaks <- function() {
  function(x) interval_breaks(nclass.FD(x))(x)
}