#' Make loaded df as rectangular-shape df
#'
#' @inheritParams readxl::read_excel
#' @export
make_rect <- function(df, range) {
  from  <- stringr::str_extract(range, "^[A-Z]+[0-9]+") %>%
    cellranger::as.cell_addr(strict = FALSE) %>%
    unclass()
  to <- stringr::str_extract(range, "[A-Z]+[0-9]+$") %>%
    cellranger::as.cell_addr(strict = FALSE) %>%
    unclass()
  t <- min(from$row, to$row)
  b <- max(from$row, to$row)
  l <- min(from$col, to$col)
  r <- max(from$col, to$col)
  df[t:b, l:r]
}

#' Fill NAs of merged columns by 'varname'
#'
#' @param row Row position of the cells to be filled by 'varname'
#' @param regex Regex matches varname for filling
unmerge_horiz <- function(df, row, regex = ".+") {
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
unmerge_vert <- function(df, col, regex = ".+") {
  out        <- df
  vars       <- dplyr::pull(df, col)
  new_row    <- stringr::str_match(vars, regex) %>%
    rep_na_rep()
  out[, col] <- new_row
  out
}

#' Remove summary rows from df
#'
#' @param key Regex to detect summary rows
#' @param colname Name of the colmun contains key
rm_sumrow <- function(df, key, colname) {
  colpos <- stringr::str_which(df[1, ], colname)
  target <- dplyr::pull(df, colpos)
  df[-stringr::str_which(target, key), ]
}

#' Remove summary cols from df
#'
#' @param key Regex to detect summary cols
#' @param rowname Name of the row contains key
rm_sumcol <- function(df, key, rowname, regex = FALSE) {
  if (regex == TRUE) {
    rowpos <- stringr::str_which(dplyr::pull(df, 1), rowname)
  } else {
    rowpos <- which(df[, 1] == rowname)
  }
  target <- dplyr::slice(df, rowpos) %>%
    unlist() %>%
    as.vector()
  df[, -stringr::str_which(target, key)]
}
