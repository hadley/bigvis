# Group

* 1d, nd

Future work: linear binning

# Summarise

* 1d

  * count, sum
  * count, mean, sd
  * median

Future work:

* skew?, kurt?
* weighted quantiles
  * sort whole vector
  * find upper and lower bounds (assuming ordered, can do incrementally)
  * interpolate between them

* 2d: cor, lm

* need to compute standard errors

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

Think about input data structure: sparse grid, represented as a coordinate list. Possible that more performance is available by switching to a sparse tensor library. 

# Visualise

* Product plots
* Standard errors + cut offs
