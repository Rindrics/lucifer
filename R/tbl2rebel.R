#' Load parameters to drive rebel() from .csv file
#'
#' @param fname file path to param sheet
load_param <- function(fname) {
  readr::read_tsv(fname, quote = "")
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

unquote <- function(x) {
  if (is.null(x)) return(NULL)
  eval(parse(text = x), envir = parent.frame(5))
}

#' Convert NA to NULL
#' @param x Object to be converted
na2null <- function(x) {
 if (is.null(x) || !is.na(x)) return(x)
 NULL
}

parse_escape <- function(x) {
  stringr::str_replace_all(x, '\\"', "'")
}

#' Drive rebel() loading parameters written in .csv file
#'
#' @param objname Name of the Object choosen from 'object' column in param sheet
#' @param paramfname Path to the parameter sheet
#' @param reportf Name of the report file to export
#' @export
tbl2rebel <- function(objname, paramfname, reportf = NULL) {
  params <- load_param(fname = paramfname) %>%
    dplyr::filter(object == as.character(objname)) %>%
    purrr::map(na2null)
  rebel(path = unquote(params$path),
        sheet_regex = unquote(params$sheet_regex),
        row_headers = unquote(params$row_headers),
        col_headers = unquote(params$col_headers),
        cluster = list(regex = unquote(params$`cluster/regex`),
                       dir = unquote(params$`cluster/dir`),
                       pos = unquote(params$`cluster/pos`),
                       offset = unquote(params$`cluster/offset`),
                       ends = list(row = unquote(params$`cluster/ends/row`),
                                   col = unquote(params$`cluster/ends/col`))
                       )
        ) %>%
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
