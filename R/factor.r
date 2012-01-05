is_factor <- function(df, vars) {
  UseMethod("is_factor")
}

is_factor.character <- function(df, vars) {
  info <- rxGetVarInfo(df, varsToKeep = vars)
  vapply(info, function(x) x$varType == "factor", logical(1))
}

is_factor.data.frame <- function(df, vars) {
  vapply(df[vars], is.factor, logical(1))
}


factor_levels <- function(df, var) {
  UseMethod("factor_levels")
}

factor_levels.data.frame <- function(df, var) {
  levels(df[[var]])
}

factor_levels.character <- function(df, var) {
  rxGetVarInfo(df, varsToKeep = var)[[1]]$levels
}
