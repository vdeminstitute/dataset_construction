// There're two types of filling for V-Dem: standard front filling by
// last observation carried forward (locf) and "interpolation" where
// because of our default date we backfill when the only observation
// is at "-12-31". We'll export each as separate functions to R for
// usability purposes, but keep the code unified here.
//
// Crucially, none of these functions deal with the actual splitting
// or ordering based on dates. We'll offload that to R.

#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
Vector<RTYPE> fill_temp(Vector<RTYPE> v, bool interpolate) {
  LogicalVector na = is_na(v);

  if (is_true(all(na)) | is_true(all((!na))))
    return v;

  int len = v.size();
  if (interpolate && is_true(all(na[seq(0, len - 2)]))) {
    v.fill(v[len - 1]);
  } else {
    for (int i = 1; i < len; i++) {
      if (Vector<RTYPE>::is_na(v[i]))
        v[i] = v[i - 1];
    }
  }

  return v;
}

SEXP fill_vector_(SEXP v, bool interpolate){
    RCPP_RETURN_VECTOR(fill_temp, v, interpolate)}

// Create a separate function for vector filling so that when we call
// the S3 method we first clone the obj and never modify the original.
//' @export
// [[Rcpp::export(name = "locf.default")]]
SEXP locf_S3vector(SEXP x) {
  SEXP out = clone(x);

  return fill_vector_(out, false);
}

//' @export
// [[Rcpp::export(name = "interpolate.default")]]
SEXP interpolate_S3vector(SEXP x) {
  SEXP out = clone(x);

  return fill_vector_(out, true);
}

template <int RTYPE>
Matrix<RTYPE> fill_matrix_temp(Matrix<RTYPE> m, bool interpolate) {
  if (m.nrow() < 2)
    return m;

  Matrix<RTYPE> out = clone(m);
  int ncols = out.ncol();
  for (int i = 0; i < ncols; i++)
    out(_, i) = fill_temp<RTYPE>(out(_, i), interpolate);

  return out;
}

//' @export
// [[Rcpp::export(name = "locf.matrix")]]
SEXP fill_S3matrix(SEXP x){RCPP_RETURN_MATRIX(fill_matrix_temp, x, false)}

//' @export
// [[Rcpp::export(name = "interpolate.matrix")]]
SEXP interpolate_S3matrix(SEXP x){RCPP_RETURN_MATRIX(fill_matrix_temp, x, true)}

//' @export
// [[Rcpp::export(name = "locf.data.frame")]]
DataFrame locf_S3df(DataFrame x) {
  if (x.nrows() < 2)
    return x;

  DataFrame out = clone(x);
  for (auto& it : out)
    it = fill_vector_(it, false);

  return out;
}

//' @export
// [[Rcpp::export(name = "interpolate.data.frame")]]
DataFrame interpolate_S3df(DataFrame x) {
  if (x.nrows() < 2)
    return x;

  DataFrame out = clone(x);
  for (auto& it : out)
    it = fill_vector_(it, true);

  return out;
}
