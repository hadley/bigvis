// How does the data structure implementing the bin affect performance.

#include <Rcpp.h>
using namespace Rcpp;

class Grouper {
    const Fast<NumericVector> x_;
    double width_;
    double origin_;
  public:
    Grouper (const NumericVector& x, double width, double origin = 0)
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

// [[Rcpp::export]]
std::vector<int> count_vector(const NumericVector& x, double width, double origin = 0) {
  Grouper grouper = Grouper(x, width, origin);
  std::vector<int> count;

  int n = grouper.size();
  for(int i = 0; i < n; ++i) {
    int bin = grouper.bin(i);
    if (bin >= count.size()) {
      count.resize(bin + 1);
    }

    ++count[bin];
  }

  return count;
}

// [[Rcpp::export]]
List count_map(const NumericVector& x, double width, double origin = 0) {
  Grouper grouper = Grouper(x, width, origin);
  std::map<int, int> count;

  int n = grouper.size();
  for(int i = 0; i < n; ++i) {
    int bin = grouper.bin(i);
    ++count[bin];
  }

  IntegerVector out_x(count.size()), out_y(count.size());
  std::map<int, int>::const_iterator count_it = count.begin(), 
    count_end = count.begin();
  for (int i = 0; count_it != count_end; ++count_it, ++i) {
    out_x[i] = count_it->first;
    out_y[i] = count_it->second;
  }
  return List::create(_["x"] = out_x, _["count"] = out_y);
}

// [[Rcpp::export]]
List count_umap(const NumericVector& x, double width, double origin = 0) {
  Grouper grouper = Grouper(x, width, origin);
  std::tr1::unordered_map<int, int> count;

  int n = grouper.size();
  for(int i = 0; i < n; ++i) {
    int bin = grouper.bin(i);

    ++count[bin];
  }

  IntegerVector out_x(count.size()), out_y(count.size());
  std::tr1::unordered_map<int, int>::iterator count_it = count.begin(), 
    count_end = count.end();
  for (int i = 0; count_it != count_end; ++count_it, ++i) {
    out_x[i] = count_it->first;
    out_y[i] = count_it->second;
  }
  return List::create(_["x"] = out_x, _["count"] = out_y);
}


template <class T>
inline void hash_combine(std::size_t & seed, const T & v) {
  std::tr1::hash<T> hasher;
  seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

namespace std {
  namespace tr1 {  
    template<typename S, typename T> struct hash<pair<S, T> > {
      inline size_t operator()(const pair<S, T> & v) const {
        size_t seed = 0;
        ::hash_combine(seed, v.first);
        ::hash_combine(seed, v.second);
        return seed;
      }
    };
  }
}

// [[Rcpp::export]]
List count_umap2(const NumericVector& x, double width, double origin = 0) {
  Grouper grouper = Grouper(x, width, origin);
  std::tr1::unordered_map<std::pair<int, int>, int> count;

  int n = grouper.size();
  for(int i = 0; i < n; ++i) {
    int bin = grouper.bin(i);

    ++count[std::make_pair(bin, bin)];
  }

  IntegerVector out_x(count.size()), out_y(count.size());
  std::tr1::unordered_map<std::pair<int, int>, int>::iterator count_it = count.begin(), 
    count_end = count.end();
  for (int i = 0; count_it != count_end; ++count_it, ++i) {
    out_x[i] = count_it->first.first;
    out_y[i] = count_it->second;
  }
  return List::create(_["x"] = out_x, _["count"] = out_y);
}

// [[Rcpp::export]]
List count_umap2_man(const NumericVector& x, double width, double origin = 0) {
  Grouper grouper = Grouper(x, width, origin);
  std::tr1::unordered_map<int, int> count;

  int n = grouper.size();
  for(int i = 0; i < n; ++i) {
    int bin = grouper.bin(i);
    bin = bin * 100 + bin;
    ++count[bin];
  }

  IntegerVector out_x(count.size()), out_y(count.size());
  std::tr1::unordered_map<int, int>::iterator count_it = count.begin(), 
    count_end = count.end();
  for (int i = 0; count_it != count_end; ++count_it, ++i) {
    out_x[i] = count_it->first;
    out_y[i] = count_it->second;
  }
  return List::create(_["x"] = out_x, _["count"] = out_y);
}


/*** R 
  options(digits = 3)
  library(microbenchmark)
  x <- runif(1e5)

  # As expected, for small contiguous inputs, vector is fastest, followed by
  # unordered maps (about half as fast), with maps in a distant last place.
  microbenchmark(
    count_vector(x, 1 / 1000),
    count_map(x, 1 / 1000),
    count_umap(x, 1 / 1000)
  )

  y <- c(x, x)
  y1 <- c(x, x + 10)
  y2 <- c(x, x + 100)
  y3 <- c(x, x + 1000)
  y4 <- c(x, x + 1000)

  # While using std::vector is somewhat faster, the asymptotic behaviour is
  # much worse - count_umap is basically constant, regardless of the number
  # of bins
  microbenchmark(
    count_vector(y, 1 / 1000),
    count_vector(y1, 1 / 1000),
    count_vector(y2, 1 / 1000),
    count_vector(y3, 1 / 1000),
    count_vector(y4, 1 / 1000),
    count_umap(y, 1 / 1000),
    count_umap(y1, 1 / 1000),
    count_umap(y2, 1 / 1000),
    count_umap(y3, 1 / 1000),
    count_umap(y4, 1 / 1000),
    times = 10
  )

  # Using umap with a pair is about twice as slow as with an int: this probably
  # implies that I should do the hashing myself.
  microbenchmark(
    count_umap(x, 1 / 1000),
    count_umap2(x, 1 / 1000),
    count_umap2_man(x, 1 / 1000)
  )

*/