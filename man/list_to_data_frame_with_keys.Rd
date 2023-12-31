% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{list_to_data_frame_with_keys}
\alias{list_to_data_frame_with_keys}
\title{Convert List of Similar Flat Sublists to a Data Frame}
\usage{
list_to_data_frame_with_keys(x, key_name, key_pattern, convert = identity)
}
\arguments{
\item{x}{list of similar flat lists, i.e. lists that have list elements with
the same names and list elements that all have length one}

\item{key_name}{name of column in the returned data frame that will contain
the integer values that are constructed from the element names in \code{x}}

\item{key_pattern}{regular expression matching all element names in \code{x}.
The expression must contain one pair of parentheses enclosing the part that
is to be used as key, e.g. \code{"element_([0-9]+)"}}

\item{convert}{function to be applied to the (character) key. Set e.g.
\code{convert = as.integer} to generate integer keys. Default:
\code{\link{identity}}}
}
\value{
data frame with keys in a column named according to \code{key_name}
and value columns according to the list elements in the sublists of
\code{x}
}
\description{
Convert List of Similar Flat Sublists to a Data Frame
}
\examples{
list_to_data_frame_with_keys(
  x = list(
    element_1 = list(a = 100, b = 10),
    element_2 = list(a = 200, b = 20)
  ),
  key_name = "element",
  key_pattern = "element_([0-9]+)",
  convert = as.integer
)
}
