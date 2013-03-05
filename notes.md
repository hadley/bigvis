# Group

* 1d (done), 2d (done), nd (done)

One day:

* 2d hex bins

Look into linear binning.

# Summarise

* all need to support weights

* 1d

  * count, sum (done)
  * count, mean, sd, (done)
  * skew?, kurt?

  * median (done)

  * quantiles
    * different to others because it takes a parameter, so will need extension to generator
    * default to R's interpolation method O(nm)
  
  * weighted quantiles
    * sort whole vector
    * find upper and lower bounds (assuming ordered, can do incrementally)
    * interpolate between them

  * histogram

* 2d
  * cor
  * lm?

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
* pre-computed grid of kernel values

Think about input data structure: sparse grid, represented as a coordinate list. Possible that more performance is available by switching to a sparse tensor library. 

# Visualise

Need to automatically compute standard errors, and provide cut off.

