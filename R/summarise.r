
groups <- list(
  Fixed = c(width = "double", origin = "double"),
  "2dFixed" = c(x_width = "double", y_width = "double",
              x_origin = "double", y_origin = "double")
)

summaries <- c(
  count = "Sum(0)",
  sum = "Sum(1)",
  mean = "Moments(1)",
  sd = "Moments(2)",
  median = "Median()"
)
summary_class <- c(
  count = "sum",
  sum = "sum",
  mean = "moments",
  sd = "moments",
  median = "median"
)
