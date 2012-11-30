# Summarise

by x, x & y, by group, hex

* bin 
  * by x and by group
  * need arbitrary breaks and fixed width bins
  * just have C++ binner class that wraps this up? only needs operator()

* summary functions: 
  * all need to support weights

  * count/sum
  * mean
  * sd
  * quantiles
  * boxplot

# Smooth

Kernel smoothing plus binned summary leads to many common statistics: density = bin + smooth, loess =~ mean + smooth, rqss =~ quantile + smooth

* smooth would expect constant bins
* single sided smoother for time data
* should probably force kernel to be odd to simplify code
* option to reflect & sum kernel values at bounds.  If TRUE length of output = length of input, otherwise = input + kernel
