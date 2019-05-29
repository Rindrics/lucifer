#' Rebel against godly Excel worksheet
#'
#' @inheritParams readxl::read_excel
#' @param row_merged Row position of merged colnames
#' @param col_merged Column position of merged rownames
rebel_sheet <- function(sheet, path, row_merged, col_merged) {
  path <- structure(path,
                    sheet = sheet,
                    row_merged = row_merged,
                    col_merged = col_merged)
  path
  attributes <- attributes(path)

  out <- load_alldata(path, sheet = sheet)
  if (attributes$row_merged > 0) {
    out <- unmerge_vert(out, col = attributes$row_merged)
  }

  if (attributes$col_merged > 0) {
    out <- unmerge_horiz(out, row = attributes$col_merged) %>%
      merge_colname(rows = 1:(attributes$col_merged + 1))
  }
  out <- headerize(out, row = 1)
}

#' Rebel against godly Excel workbook
#'
#' @inheritParams rebel_sheet
#' @param sheet.regex Regular expression to match sheetname
rebel <- function(path, sheet.regex, row_merged = 0, col_merged = 0) {
  sheets <- stringr::str_extract(readxl::excel_sheets(path), sheet.regex)
  out <- purrr::map_df(sheets, rebel_sheet, path = path,
             row_merged = row_merged,
             col_merged = col_merged)
  as.data.frame(out)
}
