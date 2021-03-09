#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

double calc_mean(NumericVector v, NumericVector d, bool na_rm) {
  LogicalVector b = is_na(v);

  if ((!na_rm && any(b).is_true()))
    return NumericVector::get_na();

  if (all(b).is_true())
    return R_NaN;

  NumericVector values = v[!b];
  NumericVector days = d[!b];

  return sum((values * days) / sum(days));
}

NumericVector daydiff(DateVector d) {
  NumericVector x = diff(d);
  Date last_date = d[d.size() - 1];

  if (last_date.format("%m-%d") == "12-31")
    x.push_back(1);
  else
    x.push_back(Date(last_date.getYear(), 12, 31) - last_date + 1);

  return x;
}

//' @export
// [[Rcpp::export(name = day_mean.data.frame)]]
DataFrame weighted_df_mean(DataFrame x, DateVector dates, bool na_rm = true) {
  if (x.nrow() != dates.size())
    stop("Unequal lengths between date vector and object");

  if (dates.size() == 0)
    return x;

  Date first_date = dates[0];
  int year = first_date.getYear();

  if (x.nrow() == 1) {
    DataFrame out = clone(x);
    out["year"] = year;

    return out;
  }

  List ls_out(x.ncol());
  NumericVector d = daydiff(dates);

  for (int i = 0; i < x.ncol(); i++) {
    if (TYPEOF(x[i]) != REALSXP && TYPEOF(x[i]) != INTSXP)
      stop("Unsupported column type");

    ls_out[i] = calc_mean(x[i], d, na_rm);
  }

  ls_out.attr("class") = "data.frame";
  ls_out.attr("names") = x.attr("names");

  ls_out["year"] = year;

  return ls_out;
}

//' @export
// [[Rcpp::export(name = day_mean.matrix)]]
NumericMatrix weighted_matrix_mean(NumericMatrix x,
                                   DateVector dates,
                                   bool na_rm = true) {
  if (x.nrow() != dates.size())
    stop("Unequal lengths between date vector and object");

  if (dates.size() == 0)
    return x;

  Date first_date = dates[0];
  int year = first_date.getYear();

  if (x.nrow() == 1) {
    NumericMatrix out = clone(x);
    rownames(out) = (StringVector)to_string(year);

    return out;
  }

  NumericMatrix m_out(1, x.ncol());
  NumericVector d = daydiff(dates);

  for (int i = 0; i < x.ncol(); i++)
    m_out(0, i) = calc_mean(x(_, i), d, na_rm);

  m_out.attr("dimnames") = List::create(year, colnames(x));
  return m_out;
}
