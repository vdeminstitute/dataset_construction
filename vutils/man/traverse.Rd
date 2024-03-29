% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coders.R
\name{traverse}
\alias{traverse}
\title{Traverse coder-country network}
\usage{
traverse(x, root = 20)
}
\arguments{
\item{x}{\code{DataFrame} long-formatted coder-level data}

\item{root}{Country_id start position}
}
\description{
For a specific C variable and from a given start country,
\code{traverse} returns a DataFrame of countries accessible through
bridge/lateral coding.
}
\details{
Ideally, for a particular C variable, every country (node)
should be linked through bridge/lateral coding. Countries which
do not appear in the output \code{DataFrame} are isolated
nodes.
}
