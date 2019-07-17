#' Load parameters to drive rebel() from .csv file
#'
#' @param fname file path to param sheet
load_param <- function(fname) {
  readr::read_csv(fname) %>%
    dplyr::mutate(`cluster/offset` = paste0("c(", `cluster/offset`, ")"))
}

#' Indicate whether the given fname has '.csv' extension
#'
#' @param fname File name to be tested
is_csv <- function(fname) {
  stringr::str_detect(fname, "\\.csv$")
}

#' Ensure the file name has specific extension
#' @param fname File name to be processed
ensure_csv <- function(fname) {
  if (is.null(fname)) return(NULL)
  if (is_csv(fname)) return(fname)
  stringr::str_replace(fname, "\\..+$", ".csv")
}

#' Drive rebel() loading parameters written in .csv file
#'
#' @param objnm Name of the Object choosen from 'object' column in param sheet
#' @param tbl_fname Path to the parameter sheet
#' @param reportf Name of the report file to export
#' @export
tbl2rebel <- function(objnm, tbl_fname, reportf = NULL) {
  params <- load_param(fname = tbl_fname)
  rebel(path = params$path, sheet_regex = params$sheet_regex,
        row_merged = params$row_merged,
        col_merged = params$col_merged,
        cluster = list(regex = params$`cluster/regex`,
                       dir = params$`cluster/dir`,
                       pos = params$`cluster/pos`,
                       offset = eval(parse(text = params$`cluster/offset`)),
                       ends = list(row = params$`cluster/ends/row`,
                                   col = params$`cluster/ends/col`))) %>%
    report(params, ensure_csv(reportf))
}

#' Concatenate df colname
#'
#' @param df Data frame to extract colname
cat_colnames <- function(df) {
  colnames(df)[stringr::str_which(colnames(df), "^(?!.*(fname|sheet)).*$")] %>%
    paste(collapse = ",")
}

#' Make summary table of rebel result
#'
#' @param df Output of \code{\link{rebel}}
#' @param params Parameter table parsed by \code{\link{load_param}}
mk_summary <- function(df, params) {
  data.frame("nrow" = nrow(df), "ncol" = ncol(df),
             "colnames" = cat_colnames(df), "params" = params)
}

#' Export result summary of rebel as csv file
#'
#' @param df Output of \code{\link{rebel}}
#' @param params Parameter table parsed by \code{\link{load_param}}
#' @param reportf Name of report file to be exported
report <- function(df, params, reportf = NULL) {
  if (is.null(reportf)) return(df)
  out <- mk_summary(df, params)
  utils::write.csv(out, file = reportf, row.names = FALSE)
  out
}
