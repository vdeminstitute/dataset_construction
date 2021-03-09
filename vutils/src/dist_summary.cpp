#include <Rcpp.h>
using namespace Rcpp;

NumericVector colwise(NumericMatrix m, double (*fn)(NumericVector)) {
  int ncol = m.ncol();
  NumericVector out(ncol);

  for (int i = 0; i < ncol; i++)
    out[i] = fn((NumericVector)m(_, i));

  return out;
}

NumericVector rowwise(NumericMatrix m, double (*fn)(NumericVector)) {
  int nrow = m.nrow();
  NumericVector out(nrow);

  for (int i = 0; i < nrow; i++)
    out[i] = fn((NumericVector)m(i, _));

  return out;
}

double wrapSD(NumericVector v) {
  return sd(v);
}

double wrapMedian(NumericVector v) {
  return median(v);
}

//' @export
// [[Rcpp::export(name = "colMedians.matrix")]]
NumericVector colMedians(NumericMatrix x) {
  return colwise(x, &wrapMedian);
}

//' @export
// [[Rcpp::export(name = "rowMedians.matrix")]]
NumericVector rowMedians(NumericMatrix x) {
  return rowwise(x, &wrapMedian);
}

//' @export
// [[Rcpp::export(name = "colSDs.matrix")]]
NumericVector colSDs(NumericMatrix x) {
  return colwise(x, &wrapSD);
}

//' @export
// [[Rcpp::export(name = "rowSDs.matrix")]]
NumericVector rowSDs(NumericMatrix x) {
  return rowwise(x, &wrapSD);
}
