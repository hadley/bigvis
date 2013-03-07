#include <bigvis.h>
using namespace Rcpp;

int BinnedVectors::bin_i(int i) const {
  int bin = 0;
  int ngroups = groups_.size();

  for (int j = 0; j < ngroups; ++j) {
    bin += groups_[j].bin_i(i) * bins_[(ngroups - 1) - j];
  }

  return bin;
}

int BinnedVectors::bin(std::vector<double> x) const {
  int ngroups = groups_.size();
  if (x.size() != ngroups) stop("x must be same length as groups");
  int bin = 0;

  for (int j = 0; j < ngroups; ++j) {
    int bin_j = groups_[j].bin(x[j]);
    bin += bin_j * bins_[(ngroups - 1) - j];
    // Rcout << "group: " << j << " bin: " << bin << " bin_j: " << bin_j << "\n";
  }

  return bin;
}

std::vector<double> BinnedVectors::unbin(int bin) const {
  int ngroups = groups_.size();
  std::vector<double> bins(ngroups);

  // if ngroups = 3, then: 
  // bin = groups[0].bin(x[0]) * bins[2] (biggest) +
  //       groups[1].bin(x[1]) * bins[1] +
  //       groups[2].bin(x[2]) * bins[0] (smallest)
  // peel off largest first
  //   bin_j = bin %/% bin[2]
  //   groups[0].unbin(bin_j)
  // and that goes in last output position

  for (int i = 0, j = ngroups - 1; i < ngroups - 1; ++i, --j) {
    int bin_j = bin % bins_[j];
    // Rcout << "group: " << j << " bin: " << bin << " bin_j: " << bin_j << "\n";
    bins[j] = groups_[j].unbin(bin_j);

    bin = (bin - bin_j) / bins_[j];
  }
  // Rcout << "group: " << 0 << " bin: " << bin << " bin_j: " << bin << "\n";
  // Special case for last group because x %% 1 = 0
  bins[0] = groups_[0].unbin(bin);

  return bins;
}

