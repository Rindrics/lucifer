#' Add columns 'fname' and 'sheet' to df
#'
#' @inheritParams make_rect
#' @param fname String to add df as column 'fname'
#' @param sheet String to add df as column 'sheet'
#' @export
add_reference <- function(df, fname, sheet) {
  dplyr::mutate(df, fname = fname, sheet = sheet)
}

#' Make df with reference for early return
#'
#' @param df Data frame to return
#' @param path File path to be used by \code{add_reference}
#' @param sheet Sheet name to be used by \code{add_reference}
#' @param funcname Name of function to create message for user
ceasefire <- function(df, path, sheet, funcname) {
  print(paste0("Good. Specify '", funcname,  "' next."))
  add_reference(df, path, sheet) %>%
    rm_nacols()
}
