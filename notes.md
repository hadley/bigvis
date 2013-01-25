# Group

* by numeric x: fixed width, arbitrary breaks (done)
* by numeric x & y: fixed width (done), hexes
  * https://github.com/d3/d3-plugins/blob/master/hexbin/hexbin.js
* by integer group (done)

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

# Smooth

Smoothing needs to be weighted so that values don't get inflated when sd is small - we're not producing densities

Kernel smoothing plus binned summary leads to many common statistics: density = bin + smooth, loess =~ mean + smooth, rqss =~ quantile + smooth

* smooth would expect constant bins
* single sided smoother for time data
* should probably force kernel to be even to simplify code
* option to reflect & sum kernel values at bounds.  If TRUE length of output = length of input, otherwise = input + kernel

# Visualise


# Syntax

    summarise1d(x, binwidth = 1 / 100)
    summarise1d(x, ,  "count", binwidth = 1 / 100)
    summarise1d(x, z, "mean", binwidth = 1 / 100)
    summarise1d(x, z, "median", binwidth = 1 / 100)
    summarise1d(x, z, weight = w, "mean", binwidth = 1 / 100)
