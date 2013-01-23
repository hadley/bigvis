# Summarise

* group 
  * by numeric x: fixed width, arbitrary breaks (done)
  * by numeric x & y: fixed width (done), hexes
    * https://github.com/d3/d3-plugins/blob/master/hexbin/hexbin.js
  * by integer group (done)

* summarise (1d)
  * all need to support weights

  * count, sum (unweighted)
  * weights, mean, var, skew?, kurt?

  * median 

  * quantiles
    * default to R's interpolation method O(nm)
  
  * weighted quantiles
    * sort whole vector
    * find upper and lower bounds (assuming ordered, can do incrementally)
    * interpolate between them

  * histogram

* summarise (2d)
  * cor
  * lm?

* smooth

* visualise

# Robustness

Median: http://people.cs.umass.edu/~mcgregor/papers/09-streamorderstatistics.pdf
Counting: count-min sketch, https://sites.google.com/site/countminsketch/
http://link.springer.com.ezproxy.rice.edu/content/pdf/10.1023%2FA%3A1023296123228
http://www.cs.virginia.edu/~son/cs851/papers/ucsb.sensys04.pdf
http://www2.research.att.com/~marioh/sketches/index.html

# Smooth

Kernel smoothing plus binned summary leads to many common statistics: density = bin + smooth, loess =~ mean + smooth, rqss =~ quantile + smooth

* smooth would expect constant bins
* single sided smoother for time data
* should probably force kernel to be odd to simplify code
* option to reflect & sum kernel values at bounds.  If TRUE length of output = length of input, otherwise = input + kernel

# Syntax

    summarise1d(x, binwidth = 1 / 100)
    summarise1d(x, "count", binwidth = 1 / 100)
    summarise1d(x, y, "mean", binwidth = 1 / 100)
    summarise1d(x, y, "median", binwidth = 1 / 100)
    summarise1d(x, weight = w, "mean", binwidth = 1 / 100)

Should return a data frame with columns left, right and count, mean, etc.

    summary1d(x, y, binwidth = 1 / 100)
    summary1d(x, "count", binwidth = 1 / 100)
