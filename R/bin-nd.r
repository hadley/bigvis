#' Efficient binning for large data.
#'
#' This function takes a data frame or xdf file as an input and produces 
#' an array of binned counts for the specified variables.  It is highly
#' optimised for the fixed bin width case.
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
#' @export
#' @examples
#' bin_1d(mtcars, "mpg", 5)
#' bin_1d(mtcars, "mpg", 5, 0)
bin_nd <- function(df, vars, binwidth = 1L, origin = NULL) {
  names(vars) <- vars

  # Determine origin, binwidth, centers Numeric variables
  numeric_vars <- vars[!is_factor(df, vars)]
  rng <- lapply(numeric_vars, var_range, df = df)
  n <- length(numeric_vars)
  
  if (is.null(origin)) {
    origin <- lapply(rng, "[", 1)
  } else {
    origin <- rep(origin, n)
    origin <- setNames(as.list(origin), numeric_vars)
  }
  
  if (is.numeric(binwidth)) {
    binwidth <- rep(binwidth, length = n)
    binwidth <- setNames(as.list(binwidth), numeric_vars)    
  }
  
  centers <- lapply(numeric_vars, function(var) {
    n <- bin_num(rng[[var]][2], binwidth[[var]], origin[[var]])
    origin[[var]] + seq(0, n) * binwidth[[var]] + binwidth[[var]] / 2
  })
  
  # Categorical variables
  factor_vars <-  setdiff(vars, numeric_vars)
  origin[factor_vars] <- NA
  binwidth[factor_vars] <- NA
  centers[factor_vars] <- lapply(factor_vars, factor_levels, df = df)
  
  nbin <- lapply(centers, length)[vars]

  counts <- fast_bin_nd(df, vars, binwidth, origin, nbin)
  dimnames(counts) <- centers[vars]
  
  structure(list(data = counts, binwidth = binwidth, origin = origin, 
    nbin = nbin, centers = centers), class = "summary_matrix")
}

fast_bin_nd <- function(df, vars, binwidth, origin, nbin) {
  UseMethod("fast_bin_nd", df)
}

#' @S3method fast_bin_nd data.frame
fast_bin_nd.data.frame <- function(df, vars, binwidth, origin, nbin) {
  ndistinct <- unlist(nbin)
  n <- prod(ndistinct)
  p <- length(nbin)

  if (n > .Machine$integer.max) stop("Too many combinations")
  
  bins <- lapply(vars, function(var) {
    if (is.na(binwidth[[var]])) {
      as.integer(df[[var]])
    } else {
      bin_num(df[[var]], binwidth[[var]], origin[[var]])
    }
  })

  combs <- c(1, cumprod(ndistinct[-p]))
  mat <- do.call("cbind", bins)
  res <- c((mat - 1L) %*% combs + 1L)
  
  counts <- tabulate(res, n)
  array(counts, unlist(nbin))  
}

#' @S3method fast_bin_nd character
fast_bin_nd.character <- function(df, vars, binwidth, origin, nbin) {
  mins <- lapply(vars, function(var) trunc(origin[[var]] / binwidth[[var]]))
  maxs <- lapply(vars, function(var) mins[[var]] + nbin[[var]])

  fs <- lapply(vars, function(var) {
    if (is.na(binwidth[[var]])) {
      substitute(~ var, list(var = as.name(var)))
    } else {
      subs <- list(var = as.name(var), min = mins[[var]], 
        max = maxs[[var]])
      substitute(~ F(var, min, max), subs)      
    }
  })
  f <- as.formula(paste(fs, collapse = ":"))
  
  width_one <- vapply(binwidth, "==", 1, FUN.VALUE = logical(1))
  transform_vars <- vars[!width_one]
  
  # Count the number in each bin      
  cubebin <- rxCube(f, data = df, means = FALSE,
    transformVars = transform_vars,
    transformFunc = bin_find_intervals(transform_vars, binwidth),
    reportProgress = 2)
  
  array(cubebin$Counts, unlist(nbin))
}

bin_find_intervals <- function(vars, binwidth) {
  function(data) {
    lapply(vars, function(var) {
      data[[var]] / binwidth[[var]]
    })
  }
}

bin_num <- function(x, binwidth, origin) {
  trunc((x - origin) / binwidth) + 1L
}
