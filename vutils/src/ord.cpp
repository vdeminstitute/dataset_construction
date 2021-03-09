#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

//' Ordinal scale transformation
//'
//' Transforms latent trait estimates into the original ordinal scale.
//'
//' @param z Extracted Z parameter as a matrix
//' @param gamma_mu Extracted gamma_mu as a matrix
//'
//' @return Integer matrix with the same dimensions as our Z matrix
//'
//' @export
// [[Rcpp::export]]
IntegerMatrix ord(NumericMatrix z, NumericMatrix gamma_mu) {
  // Binary variables
  if (gamma_mu.ncol() == 1) {
    NumericVector g = gamma_mu(_, 0);
    NumericMatrix full(z.nrow(), z.ncol());

    for (int i = 0; i < full.ncol(); i++)
      full(_, i) = z(_, i) + g;

    LogicalVector v = pnorm(full) > .5;

    IntegerMatrix out = IntegerMatrix(full.nrow(), full.ncol(), v.begin());
    colnames(out) = colnames(z);

    return out;
  }

  // Ordinal variables
  IntegerMatrix out(z.nrow(), z.ncol());

  for (int i = 0; i < z.nrow(); i++) {
    for (int j = 0; j < z.ncol(); j++) {
      // We want to find the highest threshold smaller than Z, so
      // reverse and search from end.
      LogicalVector b = rev(z(i, j) > gamma_mu(i, _));

      auto its =
          find_if(b.begin(), b.end(), [](const auto& x) { return x == TRUE; });
      int idx = distance(its, b.end());

      out(i, j) = idx;
    }
  }

  colnames(out) = colnames(z);
  return out;
}
