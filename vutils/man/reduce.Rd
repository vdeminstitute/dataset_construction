% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{reduce}
\alias{reduce}
\title{Reduce a numeric matrix}
\usage{
reduce(m, gaps, intra_year)
}
\arguments{
\item{m}{Question specific, wide-formatted matrix}

\item{gaps}{Logical vector denoting dates (rows) which are gap years}

\item{intra_year}{Logical vector denoting dates (rows) which are
intra-year observations, (i.e., coded at a month-day date
other than '12-31')}
}
\description{
Collapses a numeric matrix according to the V-Dem reduction rules.
}
\details{
\code{reduce} was written to collapse V-Dem style
wide-formatted matrices where the matrix represents a
single question with each coder as a separate column and
each row denoting a country-date. Furthermore, it assumes
that the matrix is a single country subset, is ordered
descending by date, and for simplicity that all missing
values are represented as NA (see reduce.R for more
context).

\if{html}{\out{<div class="sourceCode">}}\preformatted{      Thus, For a given matrix \code{m}, \code{reduce} will collapse
      backwards the matrix to the row (date) where there is a
      change in value within any column versus the preceding
      row (date). Changes do not include missing values; if a
      column goes from a numeric value to NA this is not
      considered a change. Furthermore, as part of the
      collapsing, missing values will be imputed backwards if
      there is no change.

      Two exceptions: gap dates (nonsequential breaks with the
      previous date by more than 1 year) are considered changes,
      and thus the boundary is preserved. Then, for intra-year
      dates values are first imputed \\emph\{forwards\} before
      subjected to the normal collapsing rules.
}\if{html}{\out{</div>}}
}
\section{Warning}{
 Ensure that the matrix is ordered descending by
date and that the order matches \code{gaps} and \code{intra_year}
}

