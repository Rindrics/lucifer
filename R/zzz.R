#' Pipe operators
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#'
#' #' @name %<>%
#' @rdname doublepipe
#' @keywords internal
#' @export
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "year", "month", "catch", "rowname"))
}
NULL
