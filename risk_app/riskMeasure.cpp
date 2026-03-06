#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix calcRiskMeasures(NumericMatrix x) {

  for (int i = 0; i < x.nrow(); i++) {

    double ytm = x(i, 0);
    double maturity = x(i, 1);
    double changeBasisPoints = x(i, 2);
    double sd = x(i, 3);
    int periods = x(i, 4);
    double fv = x(i, 5);
    double coupon = x(i, 6);

    double priceLevel = 0;

    NumericVector paymentDates(periods);

    paymentDates[0] = 0.5;

    for (int j = 1; j < periods; j++) {

      paymentDates[j] = paymentDates[j - 1] + 0.5;

    }

    for (int j = 0; j < periods; j++) {

      priceLevel += (coupon / pow((1 + ytm / 2), paymentDates[j]*2));

    }

    priceLevel += (fv / pow((1 + ytm / 2), maturity * 2));


    double stepSize = 0.0001;
    double ytmShiftUp = ytm + stepSize;
    double ytmShiftDown = ytm - stepSize;

    double priceUp = 0;
    double priceDown = 0;

    for (int j = 0; j < periods; j++) {

      priceUp += (coupon / pow((1 + ytmShiftUp / 2), paymentDates[j]*2));
      priceDown += (coupon / pow((1 + ytmShiftDown / 2), paymentDates[j]*2));

    }

    priceUp += (fv / pow((1 + ytmShiftUp / 2), maturity*2));
    priceDown += (fv / pow((1 + ytmShiftDown / 2), maturity*2));

    x(i, 7) = priceLevel;
    x(i, 8) = priceUp;
    x(i, 9) = priceDown;

    double delta = ((priceUp - priceDown) / (2 * stepSize)) / 10000;
    double gamma = ((priceUp - (2 * priceLevel) + priceDown) / (pow(stepSize, 2))) / 10000;

    x(i, 10) = delta;
    x(i, 11) = gamma;

    double risk = sd * delta;
    x(i, 12) = risk;

  }

  return x;

}
