% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strings.R
\name{rm_newline}
\alias{rm_newline}
\title{Remove new line characters}
\usage{
rm_newline(v)
}
\arguments{
\item{v}{A character vector}
}
\description{
Replaces new line characters in a character vector with a blank space
}
\examples{
v = c("Mr. Smith \\\\n goes home")
rm_newline(v)

}
