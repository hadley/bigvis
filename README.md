# bigvis

[![Travis-CI Build Status](https://travis-ci.org/hadley/bigvis.svg?branch=master)](https://travis-ci.org/hadley/bigvis)
[![Coverage Status](https://img.shields.io/codecov/c/github/hadley/bigvis/master.svg)](https://codecov.io/github/hadley/bigvis?branch=master)

The bigvis package provides tools for exploratory data analysis of __large datasets__ (10-100 million obs). The aim is to have most operations take less than 5 seconds on commodity hardware, even for 100,000,000 data points.

Since bigvis is not currently available on CRAN, the easiest way to try it out is to:

```R
# install.packages("devtools")
devtools::install_github("hadley/bigvis")
```

## Workflow

The bigvis package is structured around the following workflow:

* `bin()` and `condense()` to get a compact summary of the data

* if the estimates are rough, you might want to `smooth()`. See `best_h()` and `rmse_cvs()` to figure out a good starting bandwidth

* if you're working with counts, you might want to `standardise()`

* visualise the results with `autoplot()` (you'll need to load `ggplot2` to use this)

## Weighted statistics

Bigvis also provides a number of standard statistics efficiently implemented on weighted/binned data: `weighted.median`, `weighted.IQR`, `weighted.var`, `weighted.sd`, `weighted.ecdf` and `weighted.quantile`. 

## Installation

The package can be installed using the [`devtools`](https://github.com/hadley/devtools) package. Once `devtools` has been installed on your system, you can run the following commands in R:

    library(devtools)
    install_github("bigvis", "hadley")

Note that you will also need the [`Boost C++`](http://www.boost.org/community/cpp.html) headers and libraries installed on your system in order to compile the package.

## Acknowledgements

This package wouldn't be possible without:

* the fantastic [Rcpp](http://dirk.eddelbuettel.com/code/rcpp.html) package, which makes it amazingly easy to integrate R and C++

* JJ Allaire and Carlos Scheidegger who have indefatigably answered my many C++ questions

* the generous support of Revolution Analytics who supported the early development.

* Yue Hu, who implemented a proof of concepts that showed that it might be possible to work with this much data in R.
