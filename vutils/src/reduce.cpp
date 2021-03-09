#include <Rcpp.h>
using namespace Rcpp;

bool compare(NumericMatrix::Row x, NumericMatrix::Row y) {
  return is_true(all(na_omit(x == y)));
}

//' Reduce a numeric matrix
//'
//' Collapses a numeric matrix according to the V-Dem reduction rules.
//'
//' @param m Question specific, wide-formatted matrix
//' @param gaps Logical vector denoting dates (rows) which are gap years
//' @param intra_year Logical vector denoting dates (rows) which are
//'          intra-year observations, (i.e., coded at a month-day date
//'          other than '12-31')
//'
//' @section Warning: For something seemingly so simple, this is the '
//'                   most contentious function at V-Dem. Goodluck.
//'
//'                   Ensure that the matrix is ordered descending by
//'                   date and that the order matches \code{gaps} and
//'                   \code{intra_year}
//'
//' @details \code{reduce} was written to collapse V-Dem style
//'           wide-formatted matrices where the matrix represents a
//'           single question with each coder as a separate column and
//'           each row denoting a country-date. Furthermore, it assumes
//'           that the matrix is a single country subset, is ordered
//'           descending by date, and for simplicity that all missing
//'           values are represented as NA (see reduce.R for more
//'           context).
//'
//'           Thus, For a given matrix \code{m}, \code{reduce} will collapse
//'           backwards the matrix to the row (date) where there is a
//'           change in value within any column versus the preceding
//'           row (date). Changes do not include missing values; if a
//'           column goes from a numeric value to NA this is not
//'           considered a change. Furthermore, as part of the
//'           collapsing, missing values will be imputed backwards if
//'           there is no change.
//'
//'           Two exceptions: gap dates (nonsequential breaks with the
//'           previous date by more than 1 year) are considered changes,
//'           and thus the boundary is preserved. Then, for intra-year
//'           dates values are first imputed \emph{forwards} before
//'           subjected to the normal collapsing rules (for a full
//'           explanation see doc/MM_prep.md).
//'
//' @export
// [[Rcpp::export]]
NumericMatrix reduce(NumericMatrix m,
                     LogicalVector gaps,
                     LogicalVector intra_year) {
  int len, counter;
  len = counter = m.nrow();

  LogicalVector bools(len, true);

// Loop over rows
  for (int i = 0; i < len; i++) {
    // If 
    //      row is smaller than second last row
    //      len - 1 is last element; len -2 is second last element
    //      i < len -2 means i is third last element or higher.
    // AND
    //      intra year date
    // AND 
    //      next row is not a gap
    // AND
    //      we are in the first row OR This and the next row are different
    if (i < len - 2 && intra_year(i) && !gaps[i + 1] &&
        (i == 0 || !compare(m.row(i), m.row(i - 1)))) {
      // Loop over columns for this row and fill from next row 
      // i.e. fill from younger to older date
      for (int j = 0; j < m.row(i).size(); j++) {
        if (NumericVector::is_na(m(i, j)))
          m(i, j) = m(i + 1, j);
      }
    }

    // If
    //    not first row
    // AND
    //    no gap
    // AND
    //    no difference in row to next row
    if (i != 0 && !gaps[i] && compare(m.row(i), m.row(i - 1))) {
      // set bool for previous row to false (we do not need it)
      bools(i - 1) = false;
      // counter for number of rows to keep gets reduced
      counter--;

      // Loop over columns
      for (int j = 0; j < m.ncol(); j++) {
        // If missing element, then fill from previous row.
        // i.e. from older to younger date
        if (NumericVector::is_na(m(i, j)))
          m(i, j) = m(i - 1, j);
      }
    }
  }

  CharacterVector rns = rownames(m);

  NumericMatrix out(counter, m.ncol());
  colnames(out) = colnames(m);
  rownames(out) = rns[bools];

  int prev = 0;
  for (int i = 0; i < counter; i++) {
    for (int j = prev; j < bools.size(); j++) {
      if (bools(j)) {
        out.row(i) = m.row(j);
        prev = j + 1;
        break;
      }
    }
  }

  return out;
}
