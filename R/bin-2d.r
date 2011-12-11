#Two dimensional binning.
#This function takes a data frame or xdf file as an input and produces 
#a two dimensional array of bincounts for the specified variables, with 
#two vectors of breaks included. The "bin" variable can be either a single binwidth 
#intended for both variables, or a vector of length 2 with the desired binwidths 
#for each variable. The variable inputs must be character strings.

# bin_2d("ADS",1,1,bin)


bin_2d <- function(df, x, y, bin) {
  if (is.character(df)) { #if input is an xdf file
    info <- rxGetInfoXdf(df, getVarInfo = TRUE)
    a <- c(info$varInfo[[x]]$low, info$varInfo[[y]]$low)
    b <- c(info$varInfo[[x]]$high, info$varInfo[[y]]$high)
  } else { #input is a data frame
  	
    a <- c(min(df[[x]], na.rm = TRUE), min(df[[y]], na.rm = TRUE))
    b <- c(max(df[[x]], na.rm = TRUE), max(df[[y]], na.rm = TRUE))
  }
  
  if (length(bin) == 1) {
    bin <- rep(bin, 2)
  }
  
  xbreaks <- seq(a[[1]], b[[1]] + bin[[1]], by = bin[[1]])
  ybreaks <- seq(a[[2]], b[[2]] + bin[[2]], by = bin[[2]])

  xn <- length(xbreaks)
  yn <- length(ybreaks)
  xnbins <- xn - 1
  ynbins <- yn -1
  
  if (is.data.frame(df)) {
    xbin <- findInterval(df[[x]], xbreaks)
    ybin <- findInterval(df[[y]], ybreaks)
    bin_all <- (xbin - 1) + (ybin - 1) * (xn - 1) + 1
    xna_ind <- which(is.na(df[[x]]) == TRUE)
    yna_ind <- which(is.na(df[[y]]) == TRUE)
    both <- xna_ind[xna_ind %in% yna_ind == TRUE]
    if (length(both) == 0){
      xbin_na <- findInterval(df[[x]][is.na(df[[y]])], xbreaks)
      ybin_na <- findInterval(df[[y]][is.na(df[[x]])], ybreaks)
    }
    counts <- tabulate(bin_all,(xn - 1) * (yn - 1))
      
  }
  if (is.character(df)) {
    xform1 <- function(data){
	data$.rxRowSelection <- is.na(data[[1]])
	return(data)
	}
    rxDataStepXdf(inFile = df, outFile = "var1_NA", transformFunc = xform1, 
       transformVars = c(x,y), overwrite = TRUE)

  	xform2 <- function(data){
	data$.rxRowSelection <- is.na(data[[2]])
	return(data)
	}
    rxDataStepXdf(inFile = df, outFile = "var2_NA", transformFunc = xform2, 
       transformVars = c(x,y), overwrite = TRUE)


   #var1_NA <- rxReadXdf(file = "var1_NA", varsToKeep = c("ArrDelay","CRSDepTime","DayOfWeek"))
   #var2_NA <- rxReadXdf(file = "var2_NA", varsToKeep = c("ArrDelay","CRSDepTime","DayOfWeek"))

  	
    
    #Transformation function to be applied while reading in data    
    bin_find_interval_2d <- function(a, b, binwidth, var1, var2, xnbins, ynbins) {
      xbreaks <- seq(a[[1]], b[[1]] + binwidth[[1]], by = binwidth[[1]])
      ybreaks <- seq(a[[2]], b[[2]] + binwidth[[2]], by = binwidth[[2]])
      
      function(data) { 
        xbin <- findInterval(data[[var1]], xbreaks)
        ybin <- findInterval(data[[var2]], ybreaks)
        data$BinNumberX <- factor(xbin, levels = 1:xnbins)
        data$BinNumberY <- factor(ybin, levels = 1:ynbins)
        return(data)
      }
    }
    #Now we use the rxCube function to count the number in each bin  
      
    cubebin <- rxCube(~ BinNumberX:BinNumberY, data = df, 
      returnDataFrame = T, transformFunc = bin_find_interval_2d(a, b, bin, x, y, xnbins, ynbins), 
      transformVars = c(x, y))
    counts <- cubebin$Counts
  }
  breaks <- list("xbreaks" = xbreaks, "ybreaks" = ybreaks)
  lowhigh <- list("low" = a, "high" = b)
  counts <- array(counts, dim = c(xn - 1, yn - 1))
  list("Counts" = counts, "Breaks" = breaks, "Values" = lowhigh)
}
