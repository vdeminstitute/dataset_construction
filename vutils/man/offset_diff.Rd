% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/priors.R
\name{offset_diff}
\alias{offset_diff}
\title{Generate offsets for two groups of coders}
\usage{
offset_diff(m, target, weight, min = 1)
}
\arguments{
\item{m}{A matrix in the V-Dem wide format.}

\item{target}{Logical matrix denoting target coder group.}

\item{weight}{Numeric matrix for weighted row means (typically
coder submitted confidences).}

\item{min}{Minimum number of coders per country-date (\emph{i.e.}, rows) for the
non-target coders. The \code{min} argument allows us to restrict the calculation
of weighted mean difference to only those country-dates
(\emph{i.e.}, rows) where we have at least some minimum numbers
of non-target coders.}
}
\value{
Single numeric value that can be used to offset the ratings
for all of the target coders.
}
\description{
Given a matrix of ratings in the wide format (each column is a
separate coder and each row is a separate country-date),
\code{offset_diff} generates offsets for those coders specified in
the logical matrix \code{target} by weighted mean difference.
}
\details{
This function is exclusively used to generate offset
priors for the historical and "new" coders (\emph{i.e.}, those
who've only coded 2005 - present). The offsets are generated by
comparing the ratings of one of the aforementioned groups to
the coders not flagged by the logical \code{target} matrix.
}
\examples{
# Matrix of raw coder-level data with 3 coders where coder "a" will
# be our historical coder.
m <- matrix(1:9, 3, 3,
            dimnames = list(c("AFG 1899-01-01",
                              "AFG 1900-01-01",
                              "AFG 1901-01-01"),
                            letters[1:3]))

# Logical matrix for our single historical coder
historical <- matrix(c(rep(TRUE, 3), rep(FALSE, 6)), 3, 3)

# Confidences, for this example everyone is 100\% confident
conf <- matrix(rep(1, 9), 3, 3)

# Generate offsets for our single historical coder
offset_diff(m, historical, conf)

}
\seealso{
Other prior functions: 
\code{\link{to_seq}()},
\code{\link{weighted.rowMeans}()}
}
\concept{prior functions}
