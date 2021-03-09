#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

// template function to subset a DataFrame object by rows
template <int RTYPE>
Vector<RTYPE> subset_t(Vector<RTYPE> v, IntegerVector b) {
  return v[b];
}

SEXP subset(SEXP v, IntegerVector b) {
  RCPP_RETURN_VECTOR(subset_t, v, b);
}

//' Apply a function to a Data Frame split by year
//'
//' \code{by_year} splits a \code{DataFrame} by a \code{DateVector}
//' grouped by year and applies the supplied function to each element in
//' the resulting \code{list}.
//'
//' @param x \code{DataFrame}
//' @param dates \code{DateVector} that will be grouped by year and
//'              used to split \code{x}.
//' @param fn \code{Function} applied to each group after splitting
//'           \code{x}. The function signature is the corresponding
//'           sub-\code{DataFrame} and sub-\code{DateVector}.
//'
//' @details When splitting \code{x}, \code{by_year} will balance
//'          the groups. This means that if there's an intra-year
//'          change not on January 1st, the last observation in the
//'          previous year will be copied to January 1st. The
//'          rationale is that each group should contain the year's
//'          full codings --- especially since we usually apply the
//'          day-weighted mean function (\code{\link{day_mean}}) to
//'          each group.
//'
//' @examples
//' df <- data.frame(x = 1:3)
//' dates <- as.Date(c("1900-01-01", "1901-02-01", "1901-11-01"))
//' by_year(df, dates, list)
//'
//' @export
// [[Rcpp::export]]
List by_year(DataFrame x, DateVector dates, Function fn) {
  if (x.nrow() != dates.size())
    stop("Unequal lengths between date vector and object");

  if (any(is_na(as<NumericVector>(dates))).is_true())
    stop("Date vector contains NAs");

  DataFrame df = clone(x);

  // Start by sorting our data frame and historical_dates
  IntegerVector idx(dates.size());
  iota(idx.begin(), idx.end(), 0);

  sort(idx.begin(), idx.end(),
       [&dates](auto i, auto j) { return dates[i] < dates[j]; });

  dates = subset(dates, idx);
  for (auto& col : df)
    col = subset(col, idx);

  // Grab the years into a vector, which we'll use to determine each
  // year-based group
  IntegerVector years(dates.size());
  for (int i = 0; i < dates.size(); i++)
    years[i] = Date(dates[i]).getYear();

  // The number of unique years will be the number of -groups we return
  int group_len = unique(years).size();

  // `groups` will contain our sub data.frames and date_vectors will
  // contain the matching historical_date subsets
  List groups(group_len), date_vectors(group_len);

  // To keep track of which group we're on
  int counter = 0;

  // Points to the start of a group
  int* head = nullptr;

  for (auto its = years.begin(); its != years.end(); ++its) {
    if (!head)
      head = its;

    // Create a group and add it to our lists when the next year is a
    // new year or if we've reached the end
    if (next(its) == years.end() || *next(its) != *its) {
      size_t start = head - years.begin(), stop = its - years.begin();

      Date first_date = Date(dates[start]);
      int day = first_date.getDay(), month = first_date.getMonth();

      // Keep track if we need to add a -01-01 obs from the previous
      // year
      bool added_date = head != years.begin() && *head - *prev(head) == 1 &&
          !(day == 31 && month == 12) && !(day == 1 && month == 1);

      if (added_date)
        start--;

      // It really would be more efficient to use a range here
      IntegerVector group_idx = seq(start, stop);
      DateVector d = subset(dates, group_idx);

      List subdf(df.ncol());
      for (int j = 0; j < df.ncol(); j++)
        subdf[j] = subset(df[j], group_idx);

      subdf.attr("names") = df.attr("names");
      subdf.attr("row.names") = subset(df.attr("row.names"), group_idx);
      subdf.attr("class") = "data.frame";

      // If we've added from the previous year, change the date to
      // <year>-01-01
      if (added_date)
        d[0] = Date(1, 1, *its);

      groups[counter] = clone(subdf);
      date_vectors[counter] = clone(d);

      head = nullptr;
      counter++;
    }
  }

  List out(group_len);

  // Apply the user-supplied function to each group and return the
  // results. Don't parallelize this since this might be run inside
  // `mclapply`
  for (int i = 0; i < group_len; i++)
    out[i] = fn(groups[i], date_vectors[i]);

  return out;
}
