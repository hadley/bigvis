struct Regression {
  double alpha, beta;
};

double bisquare(double u, double b);

Regression simpleLinearRegression(const std::vector<double>& x, 
                                  const std::vector<double>& y,
                                  const std::vector<double>& w);

Regression simpleRobustRegression(const std::vector<double>& x, 
                       const std::vector<double>& y,
                       const std::vector<double>& w,
                       int iterations = 3);

double median(const std::vector<double>& x);
double median(std::vector<double>* x);
