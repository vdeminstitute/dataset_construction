#include <Rcpp.h>
using namespace Rcpp;

//' Imprint a matrix! (What does that even mean??)
//'
//' Returns a logical matrix where the elements that are not NA in the
//' columns subsetted by the given logical vector are set as true.
//'
//' @param m A numeric matrix
//' @param v A logical vector the same length as \code{ncol(m)}
//'
//' @export
// [[Rcpp::export]]
LogicalMatrix imprint(NumericMatrix m, LogicalVector v) {
  if (m.ncol() != v.size())
    stop("Arguments have non-matching lengths");

  LogicalMatrix out(m.nrow(), m.ncol());
  out.attr("dimnames") = m.attr("dimnames");

  for (int i = 0; i < out.ncol(); i++) {
    if (v[i]) {
      for (int j = 0; j < out.nrow(); j++) {
        if (m(j, i) != -1)
          out(j, i) = true;
      }
    }
  }

  return out;
}
