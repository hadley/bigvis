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

    unsigned int bin(unsigned int i) const {
      if (ISNAN(x_[i])) return 0;
      if (x_[i] < origin_) return 0;
      
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

    unsigned int bin(unsigned int i) const {
      if (IntegerVector::is_na(x_[i])) return 0;
      if (x_[i] < origin_) return 0;

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

    unsigned int bin(unsigned int i) const {
      if (ISNAN(x_[i])) return 0;

      NumericVector::iterator
        bin_it = std::upper_bound(breaks_it_, breaks_end_, x_[i]);

      // Value higher than all breaks
      if (bin_it == breaks_end_) return 0;

      return std::distance(breaks_it_, bin_it);
    }

    int size() const {
      return x_.size();
    }
};


template<typename Group>
class Group2d {
    const Group& x_;
    const Group& y_;
    int x_bins_;

  public:
    Group2d (const Group& x, const Group& y, int x_bins) 
      : x_(x), y_(y), x_bins_(x_bins) {
      if (x.size() != y.size()) {
        stop("x and y are not equal sizes");
      }
    }

    unsigned int bin(unsigned int i) const {
      int x_bin = x_.bin(i), y_bin = y_.bin(i);
      return y_bin * x_bins_ + x_bin;
    }

    int size() const {
      return x_.size();
    }
};

