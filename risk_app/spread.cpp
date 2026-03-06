#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix calcSpreads(NumericMatrix x) {

  for (int i = 0; i < x.nrow(); i++) {

    double year1 = x(i, 1);
    double year2 = x(i, 2);
    double year10 = x(i, 3);
    double year30 = x(i, 4);

    double steepSpread = year30 - year1;
    double butterflySpread = (-1 * year2) + (2 * year10) + (-1 * year30);

    x(i, 5) = steepSpread;
    x(i, 6) = butterflySpread;
  }
  return x;
}
