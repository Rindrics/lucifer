#' Fill NAs of merged column headers by left element
#'
#' @param rows Row positions of the cells to be processed
#' @param regex Regex matches varname for filling
#' @inheritParams make_rect
#' @export
fill_colhead <- function(df, rows = NULL, regex = ".+") {
  if (is.null(rows)) return(df)
  fill_colhead_ <- function(row, df, regex) {
    stringr::str_match(df[row, ], regex) %>%
      rep_na_rep() %>%
      t() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(colnames(df))
  }
  if (rows[1] == 1) {
    tibble::as_tibble(dplyr::bind_rows(purrr::map_df(rows, fill_colhead_,
                                                     df = df, regex = regex),
                                       df[-rows, ]))
  } else {
    tibble::as_tibble(dplyr::bind_rows(df[1:(rows[1] - 1), ],
                                       purrr::map_df(rows, fill_colhead_,
                                                     df = df, regex = regex),
                                       df[(max(rows) + 1):nrow(df), ]))
  }
}

#' Fill NAs of merged rows by 'varname'
#'
#' @param cols Col positions of the cells to be filled by 'varname'
#' @param regex Regex matches varname for filling
#' @inheritParams make_rect
#' @export
fill_rowhead <- function(df, cols = NULL, regex = ".+") {
  if (is.null(cols)) return(df)
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

#' Merge colnames of multiple rows
#'
#' @param rows Rows of the target colnames to be concatenated
#' @inheritParams make_rect
#' @export
merge_colname <- function(df, rows = NULL) {
  if (is.null(rows) || length(rows) == 1) return(df)
  header <- df[rows[1], ]
  body   <- df[-rows, ]
  header <- purrr::map(1:ncol(df), paste_rows, rows, df) %>%
  stringr::str_remove_all("_\\s|_NA")
  rbind(header, body)
}
