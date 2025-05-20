#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

# Let R CMD check not complain about magrittr's dot placeholder
# https://github.com/tidyverse/magrittr/issues/29
#' @importFrom utils globalVariables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}
