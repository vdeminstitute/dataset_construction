% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate_cy.R
\name{day_mean}
\alias{day_mean}
\title{Day weighted mean}
\usage{
day_mean(x, dates, na_rm = T)
}
\arguments{
\item{x}{A \code{data.frame} of numeric/integer columns or a
numeric/integer \code{matrix} object.}

\item{dates}{Date vector}

\item{na_rm}{Whether or not to remove NA values when calculating
the mean}
}
\description{
Computes the day weighted arithmetic mean of either a
\code{data.frame} or \code{matrix}. The weights are taken from the
day difference of the given \code{DateVector} and the mean is
calculated column-wise.
}
\examples{
df <- data.frame(x = c(1, 2, 3), y = c(1L, 2L, 3L))
dates <- as.Date(c("1900-01-01", "1900-03-23", "1900-12-31"))

day_mean(df, dates)

}
