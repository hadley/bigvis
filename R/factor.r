is_factor <- function(df, vars) {
  UseMethod("is_factor")
}

#' @S3method is_factor character
is_factor.character <- function(df, vars) {
  info <- rxGetVarInfo(df, varsToKeep = vars)
  vapply(info, function(x) x$varType == "factor", logical(1))
}

#' @S3method is_factor data.frame
is_factor.data.frame <- function(df, vars) {
  vapply(df[vars], is.factor, logical(1))
}


factor_levels <- function(df, var) {
  UseMethod("factor_levels")
}

#' @S3method factor_levels data.frame
factor_levels.data.frame <- function(df, var) {
  levels(df[[var]])
}

#' @S3method factor_levels character
factor_levels.character <- function(df, var) {
  rxGetVarInfo(df, varsToKeep = var)[[1]]$levels
}
