# fixed, breaks (different args)
# median,

groups <- list(
  Breaks = c(breaks = "NumericVector&"),
  Fixed = c(width = "double", origin = "double")
)
stats <- c("Sum", "Mean", "Median", "Sd")

combs <- expand.grid(group = names(groups), stat = stats)

cpp_fun <- function(stat, group) {
  name <- paste("compute", tolower(stat), tolower(group), sep = "_")

  g_vars <- groups[[group]]
  g_args <- paste(g_vars, names(g_vars), collapse = ", ")
  s_args <- paste("const NumericVector&", c("x", "y", "weight"), collapse = ", ")
  args <- paste(s_args, ", ", g_args, sep = "")

  template <- paste("<Group", group, ", ", "Stat", stat, ">", sep = "")
  body <- paste("return groupwise", template, "(y, weight, ",
    "Group", group, "(x, ", paste(names(g_vars), collapse = ", "), "));",
    sep = "")

  paste("// [[Rcpp::export]]\n",
    "NumericVector ", name, "(", args, ") {\n",
    "  ", body, "\n}\n\n", sep = "")
}


funs <- mapply(cpp_fun, combs$stat, combs$group)
cat(funs, file = "groupwise.inl", append = TRUE, sep = "")

# NumericVector compute_fixed_mean(const NumericVector& x, const NumericVector& y, const NumericVector& weight, width, origin = 0) {
#   return groupwise(y, weight, GroupFixed(x, width, origin));
# }
# compute_fixed_median
# compute_fixed_median
