struct Regression {
  double alpha, beta;
};

double bisquare(double u, double b);

Regression simpleLinearRegression(const std::vector<double>& x, 
                                  const std::vector<double>& y,
                                  const std::vector<double>& w);

double median(const std::vector<double>& x_);