// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// by_year
List by_year(DataFrame x, DateVector dates, Function fn);
RcppExport SEXP _vutils_by_year(SEXP xSEXP, SEXP datesSEXP, SEXP fnSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< DateVector >::type dates(datesSEXP);
    Rcpp::traits::input_parameter< Function >::type fn(fnSEXP);
    rcpp_result_gen = Rcpp::wrap(by_year(x, dates, fn));
    return rcpp_result_gen;
END_RCPP
}
// weighted_df_mean
DataFrame weighted_df_mean(DataFrame x, DateVector dates, bool na_rm);
RcppExport SEXP _vutils_weighted_df_mean(SEXP xSEXP, SEXP datesSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< DateVector >::type dates(datesSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_df_mean(x, dates, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// weighted_matrix_mean
NumericMatrix weighted_matrix_mean(NumericMatrix x, DateVector dates, bool na_rm);
RcppExport SEXP _vutils_weighted_matrix_mean(SEXP xSEXP, SEXP datesSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< DateVector >::type dates(datesSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_matrix_mean(x, dates, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// colMedians
NumericVector colMedians(NumericMatrix x);
RcppExport SEXP _vutils_colMedians(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(colMedians(x));
    return rcpp_result_gen;
END_RCPP
}
// rowMedians
NumericVector rowMedians(NumericMatrix x);
RcppExport SEXP _vutils_rowMedians(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rowMedians(x));
    return rcpp_result_gen;
END_RCPP
}
// colSDs
NumericVector colSDs(NumericMatrix x);
RcppExport SEXP _vutils_colSDs(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(colSDs(x));
    return rcpp_result_gen;
END_RCPP
}
// rowSDs
NumericVector rowSDs(NumericMatrix x);
RcppExport SEXP _vutils_rowSDs(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rowSDs(x));
    return rcpp_result_gen;
END_RCPP
}
// locf_S3vector
SEXP locf_S3vector(SEXP x);
RcppExport SEXP _vutils_locf_S3vector(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(locf_S3vector(x));
    return rcpp_result_gen;
END_RCPP
}
// interpolate_S3vector
SEXP interpolate_S3vector(SEXP x);
RcppExport SEXP _vutils_interpolate_S3vector(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(interpolate_S3vector(x));
    return rcpp_result_gen;
END_RCPP
}
// fill_S3matrix
SEXP fill_S3matrix(SEXP x);
RcppExport SEXP _vutils_fill_S3matrix(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fill_S3matrix(x));
    return rcpp_result_gen;
END_RCPP
}
// interpolate_S3matrix
SEXP interpolate_S3matrix(SEXP x);
RcppExport SEXP _vutils_interpolate_S3matrix(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(interpolate_S3matrix(x));
    return rcpp_result_gen;
END_RCPP
}
// locf_S3df
DataFrame locf_S3df(DataFrame x);
RcppExport SEXP _vutils_locf_S3df(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(locf_S3df(x));
    return rcpp_result_gen;
END_RCPP
}
// interpolate_S3df
DataFrame interpolate_S3df(DataFrame x);
RcppExport SEXP _vutils_interpolate_S3df(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(interpolate_S3df(x));
    return rcpp_result_gen;
END_RCPP
}
// imprint
LogicalMatrix imprint(NumericMatrix m, LogicalVector v);
RcppExport SEXP _vutils_imprint(SEXP mSEXP, SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(imprint(m, v));
    return rcpp_result_gen;
END_RCPP
}
// ord
IntegerMatrix ord(NumericMatrix z, NumericMatrix gamma_mu);
RcppExport SEXP _vutils_ord(SEXP zSEXP, SEXP gamma_muSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type z(zSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type gamma_mu(gamma_muSEXP);
    rcpp_result_gen = Rcpp::wrap(ord(z, gamma_mu));
    return rcpp_result_gen;
END_RCPP
}
// osp
NumericMatrix osp(NumericMatrix z, NumericMatrix gamma_mu);
RcppExport SEXP _vutils_osp(SEXP zSEXP, SEXP gamma_muSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type z(zSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type gamma_mu(gamma_muSEXP);
    rcpp_result_gen = Rcpp::wrap(osp(z, gamma_mu));
    return rcpp_result_gen;
END_RCPP
}
// reduce
NumericMatrix reduce(NumericMatrix m, LogicalVector gaps, LogicalVector intra_year);
RcppExport SEXP _vutils_reduce(SEXP mSEXP, SEXP gapsSEXP, SEXP intra_yearSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type gaps(gapsSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type intra_year(intra_yearSEXP);
    rcpp_result_gen = Rcpp::wrap(reduce(m, gaps, intra_year));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_vutils_by_year", (DL_FUNC) &_vutils_by_year, 3},
    {"_vutils_weighted_df_mean", (DL_FUNC) &_vutils_weighted_df_mean, 3},
    {"_vutils_weighted_matrix_mean", (DL_FUNC) &_vutils_weighted_matrix_mean, 3},
    {"_vutils_colMedians", (DL_FUNC) &_vutils_colMedians, 1},
    {"_vutils_rowMedians", (DL_FUNC) &_vutils_rowMedians, 1},
    {"_vutils_colSDs", (DL_FUNC) &_vutils_colSDs, 1},
    {"_vutils_rowSDs", (DL_FUNC) &_vutils_rowSDs, 1},
    {"_vutils_locf_S3vector", (DL_FUNC) &_vutils_locf_S3vector, 1},
    {"_vutils_interpolate_S3vector", (DL_FUNC) &_vutils_interpolate_S3vector, 1},
    {"_vutils_fill_S3matrix", (DL_FUNC) &_vutils_fill_S3matrix, 1},
    {"_vutils_interpolate_S3matrix", (DL_FUNC) &_vutils_interpolate_S3matrix, 1},
    {"_vutils_locf_S3df", (DL_FUNC) &_vutils_locf_S3df, 1},
    {"_vutils_interpolate_S3df", (DL_FUNC) &_vutils_interpolate_S3df, 1},
    {"_vutils_imprint", (DL_FUNC) &_vutils_imprint, 2},
    {"_vutils_ord", (DL_FUNC) &_vutils_ord, 2},
    {"_vutils_osp", (DL_FUNC) &_vutils_osp, 2},
    {"_vutils_reduce", (DL_FUNC) &_vutils_reduce, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_vutils(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
