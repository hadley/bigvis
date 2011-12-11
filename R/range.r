var_range <- function(df, x) {
  UseMethod("var_range", df)
}
var_range.data.frame <- function(df, x) {
  range(df[[x]], na.rm = TRUE)
}
var_range.character <- function(df, x) {
  info <- rxGetInfoXdf(df, getVarInfo = TRUE)
  c(info$varInfo[[x]]$low, info$varInfo[[x]]$high)
}

