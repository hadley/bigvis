# Group

* 1d, nd

Future work: linear binning

# Summarise

* 1d

  * count, sum
  * count, mean, sd
  * median

* 2d
  * mean
  * regression
  * robust regression

* nd 
  * mean
  * regression (with eigen or armadillo)
  * robust regression (with eigen or armadillo)

Future work:

* skew?, kurt?
* boxplot
* weighted quantiles (C++ version of R code)
* compute standard errors / bootstrap standard errors?

* infrastructure for passing multiple z
  * 2d: cor, lm
  

# Smooth

Kernel smoothing plus binned summary leads to many common statistics: density =~ bin + smooth, loess =~ mean + smooth, rqss =~ quantile + smooth

* weights
* smoothing type
  * constant
  * linear
  * robust linear (lowess)
  * (linear poisson?)
* leave-one-out cross-validation
* optimisations
  * convert to integer grid & use pre-computed grid of kernel values
  * hash in smooth_nd_1 and compute more efficiently along 1d
* deal with missing values

* smooth needs to create complete grid when factor = TRUE

Think about input data structure: sparse grid, represented as a coordinate list. Binned grid class = integer vector + width/origin/nbins (0 = NA). Most transformations break the grid, in which case all you case preserve is min, max and number of bins. All smoothing methods adapted to work in terms of these integers. Need to extract out bin/unbin into own class (initialised with std::vector of bin sizes)

Possible that more performance is available by switching to a sparse tensor library.

# Visualise

* Product plots
* Standard errors + cut offs

* Peel: implement nd version using depth