#include <Rcpp.h>
#include <boost/shared_ptr.hpp>

using namespace Rcpp;

// Wrapper for numeric vector that makes it easy figure to out which 
// bin each observation belongs to.
class BinnedVector {
    // This should probably be a const NumericVector&, but that doesn't work
    // with modules currently
    NumericVector x_;
    double width_;
    double origin_;
  public:
    BinnedVector(NumericVector x, double width, double origin = 0)
       : x_(x), width_(width), origin_(origin) {
    }

    int bin_i(int i) const {      
      return bin(x_[i]);
    }

    int bin(double x) const {
      Rcout << "Binning " << x << "\n"; 
      if (ISNAN(x) || x == INFINITY || x == -INFINITY) return 0;
      if (x < origin_) return 0;

      return (x - origin_) / width_ + 1;
    }

    double unbin(int bin) const {
      if (bin == 0) return(NAN);
      return (bin - 1) * width_ + origin_;
    }

    int nbins() const {
      return bin(max(x_));
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

    BinnedVectorReference(NumericVector x, double width, double origin = 0) {
        BinnedVector *vec = new BinnedVector(x, width, origin);
        ref = boost::shared_ptr<BinnedVector>(vec);
    }

    int bin_i(int i) const { return get()->bin_i(i); }
    int bin(double x) const { return get()->bin(x); }
    double unbin(int bin) const { return get()->unbin(bin); }
    int nbins() const { return get()->nbins(); }
};

// A data structure to store multiple binned vectors
class BinnedVectors {
    int size_;
    std::vector<BinnedVectorReference> groups_;

  public:
    std::vector<int> bins_;
    BinnedVectors () : groups_(0), bins_(0) {
    }

    void add_vector(BinnedVectorReference g) {
      int nbins = groups_.empty() ? 1 : bins_.back();

      groups_.push_back(g);
      bins_.push_back(nbins * g.nbins());
    }

    int bin_i(int i) const {
      int bin = 0;
      int ngroups = groups_.size();

      for (int j = 0; j < ngroups; ++j) {
        double bin_j = groups_[j].bin_i(i);
        bin += bin_j * bins_[j];
      }

      return bin;
    }

    int bin(std::vector<double> x) const {
      if (x.size() != groups_.size()) stop("x must be same length as groups");
      int bin = 0;

      for (int j = 0; j < x.size(); ++j) {
        bin += groups_[j].bin(x[j]) * bins_[j];
      }

      return bin;
    }

    int nbins() const {
      return bins_.back();
    }

    int size() const {
      return size_;
    }

    std::vector<double> unbin(int bin) const {
      int ngroups = groups_.size();
      std::vector<double> bins(ngroups);

      for (int j = 0; j < ngroups; ++j) {
        int bin_j = bin % bins_[j];
        bins[j] = groups_[j].unbin(bin_j);

        bin = bin - bin * bins_[j];
      }

      return bins;
    }

};

RCPP_MODULE(Binned) {
  class_<BinnedVectorReference>("Vector")
    .constructor<NumericVector, double, double>()
    .const_method("bin_i", &BinnedVectorReference::bin_i)
    .const_method("bin", &BinnedVectorReference::bin)
    .const_method("unbin", &BinnedVectorReference::unbin)
    .const_method("nbins", &BinnedVectorReference::nbins)
  ;

  class_<BinnedVectors>("Vectors")
    .constructor()
    .method("add_vector", &BinnedVectors::add_vector)
    .field("bins", &BinnedVectors::bins_)
    .const_method("bin_i", &BinnedVectors::bin_i)
    .const_method("bin", &BinnedVectors::bin)
    .const_method("unbin", &BinnedVectors::unbin)

    .const_method("nbins", &BinnedVectors::nbins)
  ;
}
RCPP_EXPOSED_AS(BinnedVectorReference);
RCPP_EXPOSED_WRAP(BinnedVectorReference);
RCPP_EXPOSED_AS(BinnedVectors);
RCPP_EXPOSED_WRAP(BinnedVectors);
