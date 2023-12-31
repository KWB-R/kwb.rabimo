% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{index_string_to_integers}
\alias{index_string_to_integers}
\title{Convert String of Integer Ranges to Vector of Integer}
\usage{
index_string_to_integers(x, splits = c(",", "-"))
}
\arguments{
\item{x}{vector of character of length one representing a string
of integer ranges, e.g. \code{"1,4-6,10-11,20"}}

\item{splits}{characters at which to 1. split \code{x} into range strings, 2.
split the range strings into begin and end values of the ranges. Default:
\code{c(",", "-")}}
}
\value{
vector of integer
}
\description{
Convert e.g. "1,4-6,10-11,20" to c(1L, 4L, 5L, 6L, 10L, 11L, 20L)
}
\examples{
index_string_to_integers("1,4-6,10-11,20")

}
