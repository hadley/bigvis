/*
 * Translated from
 * https://github.com/d3/d3-plugins/blob/master/hexbin/hexbin.js
 *
 * Copyright (C) 2013 Hadley Wickham
 * Copyright (C) 2012 Mike Bostock (mbostock at gmail dot com)
 */
class GroupHex {
    const NumericVector x_;
    const NumericVector y_;
    double x_width_;
    double x_origin_;
    double y_width_;
    double y_origin_;
    double x_bins;

  public:
    GroupHex (const NumericVector& x, const NumericVector& y, 
                double x_width, double y_width, 
                double x_origin, double y_origin, 
                double x_max)
       : x_(x), y_(y), x_width_(x_width), x_origin_(x_origin), 
          y_width_(y_width), y_origin_(y_origin) {
      if (x.size() != y.size()) stop("x & y are not the same size");
      x_bins = x_max / x_width_ + 1;
    }

    int bin_i(int i) const {
      double py = ISNAN(y_[i]) ? 0 : (y_[i] - y_origin_) / y_width_ + 1;
      int pj = py;
      double py1 = py - pj;
      
      double px = ISNAN(x_[i]) ? 0 : (x_[i] - x_origin_) / x_width_ + 1 - 
        (pj % 2 ? 0.5 : 0);
      int pi = px;

       if (fabs(py1) * 3 > 1) {
        double px1 = px - pi,
               pi2 = pi + (px < pi ? -1 : 1) / 2,
               pj2 = pj + (py < pj ? -1 : 1),
               px2 = px - pi2,
               py2 = py - pj2;
        if (px1 * px1 + py1 * py1 > px2 * px2 + py2 * py2) {
          pi = pi2 + (pj % 2 ? 1 : -1) / 2;
          pj = pj2;
        }
      }

      return pj * x_bins + pj;
    }

    int size() const {
      return x_.size();
    }
};
