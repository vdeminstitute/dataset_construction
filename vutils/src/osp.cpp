#include <Rcpp.h>

using namespace Rcpp;

//' Linearized ordinal scale transformation
//'
//' Transforms latent trait estimates into the original interval
//' scale.
//'
//' @param z Extracted \code{Z} parameter as a matrix.
//' @param gamma_mu Extracted \code{gamma_mu} as a matrix.
//'
//' @return Numeric matrix with the same dimensions as our Z matrix.
//' @export
//'
// [[Rcpp::export]]
NumericMatrix osp(NumericMatrix z, NumericMatrix gamma_mu) {
  int g_ncols = gamma_mu.ncol();

  // Binary variables
  if (g_ncols == 1) {
    NumericVector g = gamma_mu(_, 0);
    NumericMatrix full(z.nrow(), z.ncol());

    for (int i = 0; i < full.ncol(); i++)
      full(_, i) = z(_, i) + g;

    NumericVector v = pnorm(full);

    NumericMatrix out = NumericMatrix(full.nrow(), full.ncol(), v.begin());
    colnames(out) = colnames(z);

    return out;
  }

  // Ordinal variables
  NumericVector thresholds(g_ncols), P(g_ncols);
  NumericMatrix out(z.nrow(), z.ncol());

  for (int i = 0; i < z.nrow(); i++) {
    for (int j = 0; j < z.ncol(); j++) {
      thresholds = pnorm(gamma_mu(i, _) - z(i, j));

      for (int g = 0; g < g_ncols; g++) {
        if (g == g_ncols - 1)
          P[g] = (g + 1) * (1 - thresholds[g]);
        else
          P[g] = (g + 1) * (thresholds[g + 1] - thresholds[g]);
      }

      out(i, j) = sum(P);
    }
  }

  colnames(out) = colnames(z);
  return out;
}
