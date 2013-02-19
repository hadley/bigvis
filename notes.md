# Group

* 1d (done), 2d (done), nd (stuck)

One day:

* 2d hex bins

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

Need to complete code for `guess_bandwidth`. Implement varying density smoother.

Read literature on bandwidth selection for kernel smoothers. Read literature on variable bandwidth kernel smoothers.

Explore implementiong of linear kernel smooth.

`guess_bandwidth` and `smooth` should automatically pick the correct method (density estimation or smoother) based on variable name.

# Visualise

Need to automatically compute standard errors, and provide cut off.

# Data structures

Grouping classes need to be able to unbin data and generate bin positions for input data.

Summarise should be able to be sparse (e.g. use hashmap) and should generate complete output included labels (generated from group methods).

Should be able to create group objects at R level, and eliminate width & origin arguments from r functions. 