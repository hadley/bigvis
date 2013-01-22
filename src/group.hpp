#include <Rcpp.h>
using namespace Rcpp;

class GroupFixed {
    const Fast<NumericVector> x_;
    double width_;
    double origin_;
  public:
    GroupFixed (const NumericVector& x, double width, double origin = 0)
       : x_(x), width_(width), origin_(origin) {
    }

    int bin(int i) const {
      if (ISNAN(x_[i])) return 0;
      
      return (x_[i] - origin_) / width_ + 1;
    }

    int size() const {
      return x_.size();
    }
};

class GroupInteger {
    const Fast<IntegerVector> x_;
    double origin_;
  public:
    GroupInteger (const IntegerVector& x, double origin = 0) : 
        x_(x), origin_(origin) {
    }

    int bin(int i) const {
      if (IntegerVector::is_na(x_[i])) return 0;

      return x_[i] - origin_ + 1;
    }

    int size() const {
      return x_.size();
    }
};


class GroupBreaks {
    const Fast<NumericVector> x_;
    const NumericVector& breaks_;
    NumericVector::const_iterator breaks_it_, breaks_end_;

  public:
    GroupBreaks (const NumericVector& x, const NumericVector& breaks)
        : x_(x), breaks_(breaks) {
      breaks_it_ = breaks.begin();
      breaks_end_ = breaks.end();
    }

    int bin(int i) const {
      if (ISNAN(x_[i])) return 0;

      NumericVector::iterator
        bin_it = std::upper_bound(breaks_it_, breaks_end_, x_[i]);

      return std::distance(breaks_it_, bin_it);
    }

    int size() const {
      return x_.size();
    }
};



class GroupRect {
    const Fast<NumericVector> x_;
    const Fast<NumericVector> y_;
    double x_width_;
    double x_origin_;
    double y_width_;
    double y_origin_;
    double x_bins;

  public:
    GroupRect (const NumericVector& x, const NumericVector& y, 
                double x_width, double y_width, 
                double x_origin, double y_origin, 
                double x_max)
       : x_(x), y_(y), x_width_(x_width), y_width_(y_width), 
          x_origin_(x_origin), y_origin_(y_origin) {
      if (x.size() != y.size()) stop("x & y are not the same size");
      x_bins = x_max / x_width_ + 1;
    }

    int bin(int i) const {
      int x_bin = ISNAN(x_[i]) ? 0 : (x_[i] - x_origin_) / x_width_ + 1;
      int y_bin = ISNAN(y_[i]) ? 0 : (y_[i] - y_origin_) / y_width_ + 1;

      return y_bin * x_bins + x_bin;
    }

    int size() const {
      return x_.size();
    }
};
