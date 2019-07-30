#' Fill NAs of merged column headers by left element
#'
#' @param rows Row positions of the cells to be processed
#' @param regex Regex matches varname for filling
#' @inheritParams make_rect
#' @export
fill_colhead <- function(df, rows = 1, regex = ".+") {
  fill_colhead_ <- function(row, df, regex) {
    stringr::str_match(df[row, ], regex) %>%
      rep_na_rep() %>%
      t() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(colnames(df))
  }
  tibble::as_tibble(dplyr::bind_rows(purrr::map_df(rows, fill_colhead_,
                                                   df = df, regex = regex),
                                     df[-rows, ]))
}

#' Fill NAs of merged rows by 'varname'
#'
#' @param col Col position of the cells to be filled by 'varname'
#' @param regex Regex matches varname for filling
#' @inheritParams make_rect
#' @export
fill_rowhead <- function(df, cols = 1, regex = ".+") {
  fill_rowhead_ <- function(col, df, regex) {
    stringr::str_match(dplyr::pull(df, col), regex) %>%
      rep_na_rep() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(colnames(df)[col])
  }
  tibble::as_tibble(dplyr::bind_cols(purrr::map_dfc(cols, fill_rowhead_,
                                                df = df, regex = regex),
                                     df[, -cols]))
}
