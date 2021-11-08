#' Pipe operators
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#'
#' @name %<>%
#' @rdname doublepipe
#' @keywords internal
#' @export
#' @importFrom magrittr %<>%

#' @name %T>%
#' @rdname tpipe
#' @keywords internal
#' @export
#' @importFrom magrittr %T>%

#' @name :=
#' @rdname unquoeq
#' @keywords internal
#' @export
#' @importFrom rlang :=


if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "year", "month", "catch", "rowname",
                           "object"))
}
NULL
