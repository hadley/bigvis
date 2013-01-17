#include <Rcpp.h>
using namespace Rcpp;

class StatSum {
    double count;

  public:
    StatSum () : count(0) {}

    void push(double y, double w) {
      count += w;
    }

    double compute() const  {
      return count;
    }
};

class StatMean {
    double count;
    double sum;

  public:
    StatMean () : count(0), sum(0) {}

    void push(double y, double w) {
      sum += y;
      count += w;
    }

    double compute() const {
      return sum / count;
    }
};

class StatSd {
    double weight;
    double mean;
    double m2;

  public:
    StatSd () : weight(0), mean(0), m2(0) {}

    // Algorithm adapted from 
    // http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Weighted_incremental_algorithm
    void push(double y, double w) {
      double delta = y - mean;
      double R = delta * w / (weight + w);

      weight += w;
      mean += R;
      m2 += weight * delta * R;
    }

    double compute() const  {
      return m2 / weight;
    }
};

class StatMedian {
    std::vector<double> ys;

  public:
    void push(double y, double w) {
      ys.push_back(y);
    }

    // Adapted from http://stackoverflow.com/questions/1719070/
    double compute() {
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