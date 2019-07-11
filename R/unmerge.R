#' Fill NAs of merged columns by 'varname'
#'
#' @param row Row position of the cells to be filled by 'varname'
#' @param regex Regex matches varname for filling
#' @inheritParams make_rect
#' @export
unmerge_horiz <- function(df, row, regex = ".+") {
  df         <- as.data.frame(df)
  out        <- df
  vars       <- df[row, ]
  new_col    <- stringr::str_match(vars, regex) %>%
    rep_na_rep()
  out[row, ] <- new_col
  out
}

#' Fill NAs of merged rows by 'varname'
#'
#' @param col Col position of the cells to be filled by 'varname'
#' @param regex Regex matches varname for filling
#' @inheritParams make_rect
#' @export
unmerge_vert <- function(df, col, regex = ".+") {
  df         <- as.data.frame(df)
  out        <- df
  vars       <- dplyr::pull(df, col)
  new_row    <- stringr::str_match(vars, regex) %>%
    rep_na_rep()
  out[, col] <- new_row
  out
}

#' Merge colnames of multiple rows
#'
#' @param rows Rows of the target colnames to be concatenated
#' @param cols Numbers of target columns if given
#' @inheritParams make_rect
#' @export
merge_colname <- function(df, rows, cols = NULL) {
  cname   <- df[rows[1], ]
  nocname <- df[-rows, ]
  if (is.null(cols)) {
    cols <- 1:ncol(df)
  }
  cname[cols] <- purrr::map(cols, paste_rows, rows, df) %>%
  stringr::str_remove_all("_\\s|_NA")
  rbind(cname, nocname)
}
