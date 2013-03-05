#include <Rcpp.h>
#include "stats.hpp"
using namespace Rcpp;

class SummaryMoments {
    int i_;
    double weight;
    double mean;
    double m2;

  public:
    SummaryMoments (int i) : i_(i), weight(0), mean(0), m2(0) {
      if (i > 2) stop("Invalid moment");
    }

    // Algorithm adapted from 
    // http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Weighted_incremental_algorithm
    void push(double y, double w) {
      if (NumericVector::is_na(y)) return;

      // counts and weights
      weight += w;

      // mean
      if (i_ < 1) return;
      double delta = y - mean;
      mean += delta * w / weight;

      // variance
      if (i_ < 2) return;      
      m2 += delta * delta * w * (1 - w / weight);

      return;
    }

    const int size() const {
      return i_ + 1;
    }

    double compute(int i) const {
      switch (i) {
        case 0: return weight;
        case 1: return (weight == 0) ? NAN : mean;
        case 2: return (weight == 0) ? NAN : pow(m2 / (weight - 1), 0.5);
        default: 
          stop("Invalid output requested");
          return NAN;
      }
    }

    std::string name(int i) const {
      switch (i) {
        case 0: return "count";
        case 1: return "mean";
        case 2: return "sd";
        default: 
          stop("Invalid output requested");
          return "";
      }
    }
};

class SummarySum {
    int i_;
    int weight;
    double sum;

  public:
    SummarySum (int i) : i_(i), weight(0), sum(0) {
      if (i > 1 || i < 0) stop("Invalid moment");
    }

    void push(double y, double w) {
      if (NumericVector::is_na(y)) return;

      weight += w;
      if (i_ < 1) return;

      sum += y * w;
    }

    const int size() const {
      return i_ + 1;
    }

    double compute(int i) const  {
      switch (i) {
        case 0: return weight;
        case 1: return sum;
        default: 
          stop("Invalid output requested");
          return NAN;
      }
    }

    std::string name(int i) const {
      switch (i) {
        case 0: return "count";
        case 1: return "sum";
        default: 
          stop("Invalid output requested");
          return "";
      }
    }

};

class SummaryMedian {
    std::vector<double> ys;

  public:
    void push(double y, double w) {
      if (NumericVector::is_na(y)) return;

      ys.push_back(y);
    }

    int size() const {
      return 1;
    }

    // Adapted from http://stackoverflow.com/questions/1719070/
    double compute(int i) {
      return median(&ys);
    }

    std::string name(int i) const {
      return "median";
    }
};
