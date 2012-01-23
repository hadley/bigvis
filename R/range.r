var_range <- function(df, x) {
  UseMethod("var_range", df)
}

#' @S3method var_range data.frame
var_range.data.frame <- function(df, x) {
  range(df[[x]], na.rm = TRUE)
}

#' @S3method var_range character
var_range.character <- function(df, x) {
  info <- rxGetInfoXdf(df, getVarInfo = TRUE)
  c(info$varInfo[[x]]$low, info$varInfo[[x]]$high)
}

