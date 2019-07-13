#' Add columns 'fname' and 'sheet' to df
#'
#' @inheritParams make_rect
#' @param path String to add df as column 'fname'
#' @param sheet String to add df as column 'sheet'
#' @export
add_reference <- function(df, path = NULL, sheet = NULL) {
  if (is.null(path) || is.null(sheet)) return(df)
  dplyr::mutate(df, fname = path, sheet = sheet)
}


#' Make df with reference for early return
#'
#' @param df Data frame to return
#' @param path File path to be used by \code{add_reference}
#' @param sheet Sheet name to be used by \code{add_reference}
#' @param funcname Name of function to create message for user
#' @param posnames If TRUE, row- and col- names are set to 'pos=' format for
#' following cluster extraction
ceasefire <- function(df, path = NULL, sheet = NULL,
                      funcname, posnames = TRUE) {
  head10 <- function(x) {
    print(utils::head(x, 10))
  }
  message(paste0("Good. Specify '", funcname, "' next."))
  if (posnames == TRUE) {
    df %>%
      magrittr::set_colnames(paste0("(dir='v',pos=", 1:ncol(df), ")")) %>%
      dplyr::mutate(rowname = paste0("(dir='h',pos=", 1:nrow(df), ")")) %>%
      add_reference(path, sheet) %>%
      tibble::column_to_rownames("rowname") %T>%
      head10() %>%
      return()
  } else {
    add_reference(df, path, sheet)
  }
}
