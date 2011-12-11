#' Efficient 1d binning for large data.
#'
#' This function takes a data frame or xdf file as an input and produces 
#' a vector of bin counts for the specified variable.  It is highly optimised
#' for the fixed bin width case.
#' 
#' @param df Either a data frame, or the name of a xdf file.
#' @param x Index of variable of interest (either name or numeric position)
#' @param binwidth Width of bin.
#' @param origin Location to start first bin.  If missing, defaults to
#'   minimum of x variable.
#' @return A list with components \itemize{
#'   \item \code{count}: an integer vector of counts. Will be of length
#'      \code{nbin + 1}: the final bin is the number of missing values.
#'   \item \code{range}: a numeric vector giving the range of the data
#'   \item \code{nbin}: a single integer giving the number of bins
#' }
#' @examples
#' bin_1d(mtcars, "mpg", 5)
#' bin_1d(mtcars, "mpg", 5, 0)
bin_1d <- function(df, x, binwidth, origin = NULL) { 
  rng <- var_range(df, x)

  if (is.null(origin)) {
    origin <- rng[1]
  } else if (origin > rng[1]) {
    stop("Origin must be less than or equal to the minimum of data")
  }
  
  stopifnot(length(binwidth == 1) && is.numeric(binwidth))
    
  nbin <- bin_num(rng[2], binwidth, origin)
  counts <- fast_bin_1d(df, x, binwidth, origin, nbin)
  
  list(count = counts, range = c(origin, rng[2]), nbin = nbin)
}

fast_bin_1d <- function(df, x, binwidth, origin, nbin) {
  UseMethod("fast_bin_1d", df)
}
fast_bin_1d.character <- function(df, x, binwidth, origin, nbin) {

  # Create new variable giving bin number
  bin_find_interval <- function(x, breaks) {
    function(data) { 
      binnumber <- bin_num(data[[x]], binwidth, origin)
      data$.bin <- factor(binnumber, levels = c(seq_len(nbin), NA))
      return(data)
    }
  }

  # Count the number in each bin      
  cubebin <- rxCube(~ .bin, data = df, returnDataFrame = TRUE, 
    transformFunc = bin_find_interval(x, breaks), transformVars = x,
    reportProgress = 0)
    
  xform <- function(data,x){
    data$.rxRowSelection <- is.na(data[[1]])
    return(data)
  }
  rxDataStepXdf(inFile = df, outFile = "var_NA", transformFunc = xform, 
     transformVars = x, overwrite = TRUE)
  na_info <- rxGetInfoXdf("var_NA", getVarInfo = TRUE)
  counts <- c(cubebin$Counts, na_info[[2]])
}

fast_bin_1d.data.frame <- function(df, x, binwidth, origin, nbin) {
  bin <- bin_num(df[[x]], binwidth, origin)
  c(tabulate(bin, nbin), sum(is.na(bin)))
}

bin_num <- function(x, binwidth, origin) {
  (x - origin) / binwidth + 1
}
