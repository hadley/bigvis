#include <Rcpp.h>
using namespace Rcpp;

NumericVector frange(const NumericVector& x, const bool finite = true);

class GroupFixed {
    const NumericVector& x_;
    double width_;
    double origin_;
  public:
    GroupFixed (const NumericVector& x, double width, double origin = 0)
       : x_(x), width_(width), origin_(origin) {
    }

    int bin(int i) const {
      if (ISNAN(x_[i]) || x_[i] == INFINITY || x_[i] == -INFINITY) return 0;
      if (x_[i] < origin_) return 0;
      
      return bin(x_[i]);
    }

    int bin(double x) const {
      return (x - origin_) / width_ + 1;
    }

    double unbin(int bin) const {
      if (bin == 0) return(NAN);
      return (bin - 1) * width_ + origin_;
    }


    int size() const {
      return x_.size();
    }

    int nbins() const {
      double max = frange(x_)(1);
      double dest = floor((max - origin_) / width_) * width_ + origin_;

      // + 1 for missing values
      // + 1 if highest value is on right-open boundary
      return (dest - origin_) / width_ + 1 + ((max >= dest) ? 1 : 0);
    }

};

template<typename Group>
class Group2d {
    const Group& x_;
    const Group& y_;
    int x_bins_;
    int y_bins_;

  public:
    Group2d (const Group& x, const Group& y) : x_(x), y_(y) {
      if (x_.size() != y_.size()) {
        stop("x and y are not equal sizes");
      }
      x_bins_ = x_.nbins();
      y_bins_ = y_.nbins();

      // Rcout << "x_bins: " << x_bins_ << " y_bins: " << y_bins_ << "\n";
    }

    int bin(int i) const {
      int x_bin = x_.bin(i), y_bin = y_.bin(i);
      int bin = y_bin * x_bins_ + x_bin;
      // Rcout << i << ": (" << x_bin << "," << y_bin << ") -> " << bin << "\n";
      return bin;
    }

    int size() const {
      return x_.size();
    }

    int nbins() const {
      return x_bins_ * y_bins_;
    }
};


template<typename Group>
class GroupNd {
    const std::vector<Group> groups_;
    const int ngroups_;

    int size_;
    std::vector<int> bins_;

  public:
    GroupNd (const std::vector<Group> groups) 
        : groups_(groups), ngroups_(groups.size()) {
      if (groups.size() == 0) {
        stop("Empty groups vector passed to GroupCompound");
      }

      size_ = groups[0].size();

      bins_[0] = 1;
      for (int i = 0; i < ngroups_ - 1; ++i) {
        if (groups_[i].size() != size_) stop("Groups not equal sizes");

        bins_[i + 1] = bins_[i] * groups_[i].nbins(); 
      }
    }

    int bin(int i) const {
      int bin = 0;

      for (int j = 0; j < ngroups_; ++j) {
        bin += groups_[j].bin(i) * bins_[j];
      }

      return bin;
    }

    // int nbins() const {
    //   return bins_[ngroups_ - 1];
    // }

    int ngroups() const {
      return groups_.size();
    }

    int size() const {
      return size_;
    }

    std::vector<double> unbin(int bin) const {
      std::vector<double> bins(ngroups_);

      for (int j = 0; j < ngroups_; ++j) {
        int bin_j = bin % bins_[j];
        bins[j] = groups_[j].unbin(bin_j);

        bin = bin - bin * bins_[j];
      }

      return bins;
    }

};
