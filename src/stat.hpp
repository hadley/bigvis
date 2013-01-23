#include <Rcpp.h>
using namespace Rcpp;

class StatMoments {
    int i_;
    double weight;
    double mean;
    double m2;

  public:
    StatMoments (int i) : i_(i), weight(0), mean(0), m2(0) {
      if (i > 2) stop("Invalid moment");
    }

    // Algorithm adapted from 
    // http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Weighted_incremental_algorithm
    void push(double y, double w) {
      // counts and weights
      weight += w;

      // mean
      if (i_ < 1) return;
      double delta = y - mean;
      double R = delta * w / (weight);
      mean += R;

      // variance
      if (i_ < 2) return;      
      m2 += weight * delta * R;

      return;
    }

    int size() {
      return i_ + 1;
    }

    double compute(int i) const {
      switch (i) {
        case 0: return weight;
        case 1: return mean;
        case 2: return m2 / weight;
        default: stop("Invalid output requested");
      }
    }
};

class StatSum {
    int i_;
    int weight;
    double sum;

  public:
    StatSum (int i) : i_(i), weight(0), sum(0) {
      if (i > 1 || i < 0) stop("Invalid moment");
    }

    void push(double y, double w) {
      weight += w;
      if (i_ < 1) return;

      sum += y * w;
    }

    int size() {
      return i_ + 1;
    }

    double compute(int i) const  {
      switch (i) {
        case 0: return weight;
        case 1: return sum;
        default: stop("Invalid output requested");
      }
    }
};

class StatMedian {
    std::vector<double> ys;

  public:
    void push(double y, double w) {
      ys.push_back(y);
    }

    int size() {
      return 1;
    }

    // Adapted from http://stackoverflow.com/questions/1719070/
    double compute(int i) {
      if (ys.empty()) return NAN;

      int size = ys.size();
      std::vector<double>::iterator upper = ys.begin() + (int) (size / 2);
      std::nth_element(ys.begin(), upper, ys.end());

      if (size % 2 == 1) {
        return *upper;
      } else {
        std::vector<double>::iterator lower = upper - 1;
        std::nth_element(ys.begin(), lower, upper);
        return (*upper + *lower) / 2.0;
      }
    }
};
