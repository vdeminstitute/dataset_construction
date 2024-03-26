#' Apply a function to a Data Frame split by year
#'
#' \code{by_year} splits a \code{DataFrame} by a \code{DateVector}
#' grouped by year and applies the supplied function to each element in
#' the resulting \code{list}.
#'
#' @param x \code{DataFrame}
#' @param dates \code{DateVector} that will be grouped by year and
#'              used to split \code{x}.
#' @param fn \code{Function} applied to each group after splitting
#'           \code{x}. The function signature is the corresponding
#'           sub-\code{DataFrame} and sub-\code{DateVector}.
#'
#' @details When splitting \code{x}, \code{by_year} will balance
#'          the groups. This means that if there's an intra-year
#'          change not on January 1st, the last observation in the
#'          previous year will be copied to January 1st. The
#'          rationale is that each group should contain the year's
#'          full codings --- especially since we usually apply the
#'          day-weighted mean function (\code{\link{day_mean}}) to
#'          each group.
#'
#' @examples
#' df <- data.frame(x = 1:3)
#' dates <- as.Date(c("1900-01-01", "1901-02-01", "1901-11-01"))
#' by_year(df, dates, list)
#'
#' @export
by_year <- function(x, dates, fn) {
    .Call('_vutils_by_year', PACKAGE = 'vutils', x, dates, fn)
}

#' @export
day_mean.data.frame <- function(x, dates, na_rm = TRUE) {
    .Call('_vutils_weighted_df_mean', PACKAGE = 'vutils', x, dates, na_rm)
}

#' @export
day_mean.matrix <- function(x, dates, na_rm = TRUE) {
    .Call('_vutils_weighted_matrix_mean', PACKAGE = 'vutils', x, dates, na_rm)
}

#' @export
colMedians.matrix <- function(x) {
    .Call('_vutils_colMedians', PACKAGE = 'vutils', x)
}

#' @export
rowMedians.matrix <- function(x) {
    .Call('_vutils_rowMedians', PACKAGE = 'vutils', x)
}

#' @export
colSDs.matrix <- function(x) {
    .Call('_vutils_colSDs', PACKAGE = 'vutils', x)
}

#' @export
rowSDs.matrix <- function(x) {
    .Call('_vutils_rowSDs', PACKAGE = 'vutils', x)
}

#' @export
locf.default <- function(x) {
    .Call('_vutils_locf_S3vector', PACKAGE = 'vutils', x)
}

#' @export
interpolate.default <- function(x) {
    .Call('_vutils_interpolate_S3vector', PACKAGE = 'vutils', x)
}

#' @export
locf.matrix <- function(x) {
    .Call('_vutils_fill_S3matrix', PACKAGE = 'vutils', x)
}

#' @export
interpolate.matrix <- function(x) {
    .Call('_vutils_interpolate_S3matrix', PACKAGE = 'vutils', x)
}

#' @export
locf.data.frame <- function(x) {
    .Call('_vutils_locf_S3df', PACKAGE = 'vutils', x)
}

#' @export
interpolate.data.frame <- function(x) {
    .Call('_vutils_interpolate_S3df', PACKAGE = 'vutils', x)
}

#' Returns a logical matrix where the elements that are not NA in the
#' columns subsetted by the given logical vector are set as true.
#'
#' @param m A numeric matrix
#' @param v A logical vector the same length as \code{ncol(m)}
#'
#' @export
imprint <- function(m, v) {
    .Call('_vutils_imprint', PACKAGE = 'vutils', m, v)
}

#' Ordinal scale transformation
#'
#' Transforms latent trait estimates into the original ordinal scale.
#'
#' @param z Extracted Z parameter as a matrix
#' @param gamma_mu Extracted gamma_mu as a matrix
#'
#' @return Integer matrix with the same dimensions as our Z matrix
#'
#' @export
ord <- function(z, gamma_mu) {
    .Call('_vutils_ord', PACKAGE = 'vutils', z, gamma_mu)
}

#' Linearized ordinal scale transformation
#'
#' Transforms latent trait estimates into the original interval
#' scale.
#'
#' @param z Extracted \code{Z} parameter as a matrix.
#' @param gamma_mu Extracted \code{gamma_mu} as a matrix.
#'
#' @return Numeric matrix with the same dimensions as our Z matrix.
#' @export
#'
osp <- function(z, gamma_mu) {
    .Call('_vutils_osp', PACKAGE = 'vutils', z, gamma_mu)
}

#' Reduce a numeric matrix
#'
#' Collapses a numeric matrix according to the V-Dem reduction rules.
#'
#' @param m Question specific, wide-formatted matrix
#' @param gaps Logical vector denoting dates (rows) which are gap years
#' @param intra_year Logical vector denoting dates (rows) which are
#'          intra-year observations, (i.e., coded at a month-day date
#'          other than '12-31')
#'
#' @section Warning: Ensure that the matrix is ordered descending by
#'          date and that the order matches \code{gaps} and \code{intra_year}
#'
#' @details \code{reduce} was written to collapse V-Dem style
#'           wide-formatted matrices where the matrix represents a
#'           single question with each coder as a separate column and
#'           each row denoting a country-date. Furthermore, it assumes
#'           that the matrix is a single country subset, is ordered
#'           descending by date, and for simplicity that all missing
#'           values are represented as NA (see reduce.R for more
#'           context).
#'
#'           Thus, For a given matrix \code{m}, \code{reduce} will collapse
#'           backwards the matrix to the row (date) where there is a
#'           change in value within any column versus the preceding
#'           row (date). Changes do not include missing values; if a
#'           column goes from a numeric value to NA this is not
#'           considered a change. Furthermore, as part of the
#'           collapsing, missing values will be imputed backwards if
#'           there is no change.
#'
#'           Two exceptions: gap dates (nonsequential breaks with the
#'           previous date by more than 1 year) are considered changes,
#'           and thus the boundary is preserved. Then, for intra-year
#'           dates values are first imputed \emph{forwards} before
#'           subjected to the normal collapsing rules.
#'
#' @export
reduce <- function(m, gaps, intra_year) {
    .Call('_vutils_reduce', PACKAGE = 'vutils', m, gaps, intra_year)
}

