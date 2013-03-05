#include <algorithm>
#include <Rcpp.h>
#include "group.hpp"
using namespace Rcpp;

//' Smooth an n-dimensional condensed dataset with 
// [[Rcpp::export]]
NumericVector smooth_nd_1(const NumericMatrix& grid_in, 
                          const NumericVector& z_in, 
                          const NumericMatrix& grid_out, 
                          const int var, const double h) {

  if (var < 0) stop("var < 0");
  if (var >= grid_in.ncol()) stop("var too large");
  if (h <= 0) stop("h <= 0");
  if (grid_in.nrow() != z_in.size()) stop("Incompatible input lengths");
  if (grid_in.ncol() != grid_out.ncol()) stop("Incompatible grid sizes");

  int n_in = grid_in.nrow(), n_out = grid_out.nrow(), d = grid_in.ncol();
  NumericVector z_out(n_out), w_out(n_out);

  // Will be much more efficient to slice up by input dimension:
  // and most efficient way of doing that will be to bin with / bw
  // My data structure: sparse grids

  for (int i = 0; i < n_in; ++i) {
    for(int j = 0; j < n_out; ++j) {
      // Check that all variables (apart from var) are equal
      bool equiv = true;
      for (int k = 0; k < d; ++k) {
        if (k == var) continue;
        if (grid_in(i, k) != grid_out(j, k)) {
          equiv = false;
          break;
        }
      };
      if (!equiv) continue;

      double dist = (grid_in(i, var) - grid_out(j, var)) / h;
      double k = R::dnorm(dist, 0, 1, 0);
      w_out[j] += k;
      z_out[j] += z_in[i] * k;
    }
  }

  for(int j = 0; j < n_out; ++j) {
    z_out[j] /= w_out[j];
  }

  return z_out;
}

/*** R
library(ggplot2)

grid <- as.matrix(expand.grid(x = 1:10, y = 1:10, KEEP.OUT.ATTRS = FALSE))

z <- rep(0, nrow(grid))
z[c(5, 23, 84)] <- 1

qplot(grid[, 1], grid[, 2], fill = z, geom = "raster")

z_x <- smooth_nd_1(grid, z, grid, 0, 1)
z_y <- smooth_nd_1(grid, z, grid, 1, 1)

qplot(grid[, 1], grid[, 2], fill = z_x, geom = "raster")
qplot(grid[, 1], grid[, 2], fill = z_y, geom = "raster")

z_xy <- smooth_nd_1(grid, z_x, grid, 1, 1)
z_yx <- smooth_nd_1(grid, z_y, grid, 0, 1)

qplot(grid[, 1], grid[, 2], fill = z_xy, geom = "raster")
qplot(grid[, 1], grid[, 2], fill = z_yx, geom = "raster")

z2 <- smooth2d_full(grid[, 1], grid[, 2], z, unique(grid[, 1]), unique(grid[, 2]), 1, 1)
image(z2, useRaster = T)

x <- runif(1e5)
xsum <- condense(bin(x, 1/100))[-1, ]

gridx <- matrix(xsum$x, ncol = 1)
smooth_nd_1(gridx, xsum$.count, gridx, 0, 0.1)

*/