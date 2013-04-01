#include <Rcpp.h>
#include <boost/shared_ptr.hpp>

using namespace Rcpp;

// Wrapper for numeric vector that makes it easy figure to out which 
// bin each observation belongs to.
class BinnedVector {
    // This should probably be a const NumericVector&, but that doesn't work
    // with modules currently
    NumericVector x_;
    String name_;
    double width_;
    double origin_;
  public:
    BinnedVector(NumericVector x, String name, double width, double origin = 0)
       : x_(x), name_(name), width_(width), origin_(origin) {
    }

    int bin_i(int i) const {
      return bin(x_[i]);
    }

    int bin(double x) const {
      if (ISNAN(x) || x == INFINITY || x == -INFINITY) return 0;
      if (x < origin_) return 0;

      return (x - origin_) / width_ + 1;
    }

    double unbin(int bin) const {
      if (bin == 0) return(NA_REAL);
      return (bin - 1) * width_ + origin_ + width_ / 2;
    }

    int nbins() const;

    int size() const {
      return x_.size();
    }

    double origin() const {
      return origin_;
    } 

    double width() const {
      return width_;
    }

    String name() const {
      return name_;
    }

};

// This class is just boilerplate. There might be rcpp magic that does the right thing here
// but I don't know it.
class BinnedVectorReference {
    boost::shared_ptr<BinnedVector> ref;

    const BinnedVector *get() const {
        return ref.get();
    };
    BinnedVector *get() {
        return ref.get();
    };

public:
    BinnedVectorReference() {};

    BinnedVectorReference(const BinnedVectorReference &o):
        ref(o.ref) {};

    explicit BinnedVectorReference(BinnedVector *ptr) {
        // Watch out, this takes ownership of the pointer!
        ref = boost::shared_ptr<BinnedVector>(ptr);
    }

    BinnedVectorReference(NumericVector x, String name, double width, double origin = 0) {
        BinnedVector *vec = new BinnedVector(x, name, width, origin);
        ref = boost::shared_ptr<BinnedVector>(vec);
    }

    int bin_i(int i) const { return get()->bin_i(i); }
    int bin(double x) const { return get()->bin(x); }
    double unbin(int bin) const { return get()->unbin(bin); }
    int nbins() const { return get()->nbins(); }
    int size() const { return get()->size();}
    double origin() const { return get()->origin();}
    double width() const { return get()->width();}
    String name() const { return get()->name();}
};

// A data structure to store multiple binned vectors
class BinnedVectors {
    int size_;
    std::vector<BinnedVectorReference> groups_;

  public:
    std::vector<int> bins_;
    BinnedVectors () : groups_(0), bins_(0) {
    }

    BinnedVectors (List gs) : groups_(0), bins_(0) {
      int n = gs.size();
      for (int i = 0; i < n; ++i) {
        add_vector(as<BinnedVectorReference>(gs[i]));
      }
    }

    void add_vector(BinnedVectorReference g) {
      if (groups_.empty()) {
        bins_.push_back(1);
        size_ = g.size();
      } else {
        if (g.size() != size_) stop("Inconsistent sizes");
        bins_.push_back(bins_.back() * g.nbins());
      }
      groups_.push_back(g);

    }

    int bin_i(int i) const;
    int bin(std::vector<double> x) const;
    std::vector<double> unbin(int bin) const;
    
    int nbins() const {
      return bins_.back() * groups_.front().nbins();
    }

    int ngroups() const {
      return bins_.size();
    }

    int size() const {
      return size_;
    }

    String name(int j) const {
      return groups_[j].name();
    }

};


RCPP_EXPOSED_AS(BinnedVectorReference)
RCPP_EXPOSED_WRAP(BinnedVectorReference)
RCPP_EXPOSED_AS(BinnedVectors)
RCPP_EXPOSED_WRAP(BinnedVectors)
