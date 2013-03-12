"%||%" <- function(x, y) if (is.null(x)) y else x

last <- function(x) x[length(x)]

"%contains%" <- function(df, var) {
  var %in% names(df)
}

find_fun <- function(name, env = globalenv()) {
  if (is.function(name)) return(name)

  ns_env <- asNamespace("bigvis")
  if (exists(name, ns_env, mode = "function")) {
    return(get(name, ns_env))
  }

  if (exists(name, env, mode = "function")) {
    return(get(name, env))
  }

  stop("Could not find function ", name, call. = FALSE)
}
