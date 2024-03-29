% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{make_900cols}
\alias{make_900cols}
\title{Copy column 900 times}
\usage{
make_900cols(df, var, interpolate = FALSE)
}
\arguments{
\item{df}{Input data.frame that should contain the column of interest
and case identifiers: country_text_id and historical date}

\item{var}{Variable to be copied 900 times}
}
\description{
\code{make_900cols} returns the data frame ready for BFA.
The first column contains the identifiers.
}
