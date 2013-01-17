class StatMean {
    double count;
    double sum;

  public:
    StatMean () : count(0), sum(0) {}

    void push(double y, double weight) {
      sum += y;
      count += w;
    }

    double compute() {
      return sum / count;
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