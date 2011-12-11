#' This function takes a data frame or xdf file as an input and produces 
#' an array of bincounts for the specified variable, with the vector of breaks
#' included.
#' 
#' @param df Data frame 
#' @param x Index of variable of interest (either name or numeric position)
#' @param bin a specific binwidth, or a vector of breaks.
bin_1d <- function(df, x, bin) { 
  
  # Figure out range of variable
  if (is.character(df)) { 
    info <- rxGetInfoXdf(df, getVarInfo = TRUE)
    a <- info$varInfo[[x]]$low
    b <- info$varInfo[[x]]$high
  } else { #input is a data frame
    a <- min(df[[x]], na.rm = TRUE)
    b <- max(df[[x]], na.rm = TRUE)
  }

  if (length(bin) == 1) { 
    # "bin" is a specified binwidth
    breaks <- seq(a, b + bin, by = bin)
  } else {
    breaks <- bin
  }
  
  nbin <- length(breaks) - 1
  
  if (min(breaks) > a || max(breaks) < b) stop("Breaks do not span full range of data")
  
  if (is.character(df)) { #For xdf files with RevoScaleR package

    # Create new variable giving bin number
    bin_find_interval <- function(x, breaks) {
      function(data) { 
        binnumber <- findInterval(data[[x]], breaks)
        data$.bin <- factor(binnumber, levels = c(seq_along(breaks), NA))
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
  } else if (is.data.frame(df)) {
     binnumber <- findInterval(df[[x]], breaks)     
     counts <- c(tabulate(binnumber, length(breaks) - 1), 
      sum(is.na(binnumber)))
  }

  # The output of this function is an array of counts, along with the vector
  # of breaks.
  lowhigh <- c(a, b)
  names(lowhigh) <- c("low", "high")
  list("Counts" = counts, "Breaks" = breaks, "Values" = lowhigh) 
}



