#' Add columns 'fname' and 'sheet' to df
#'
#' @inheritParams make_rect
#' @param fname String to add df as column 'fname'
#' @param sheet String to add df as column 'sheet'
add_reference <- function(df, fname, sheet) {
  dplyr::mutate(df, fname = fname, sheet = sheet)
}
