#' Rebel against godly Excel worksheet
#'
#' @inheritParams readxl::read_excel
#' @param row_merged Row position of merged colnames
#' @param col_merged Column position of merged rownames
#' @param cluster List of cluster, direction, pos
rebel_sheet <- function(sheet, path, row_merged = 0, col_merged = 0,
                        cluster = NULL) {
  path <- structure(path,
                    sheet = sheet,
                    row_merged = row_merged,
                    col_merged = col_merged,
                    cluster = cluster)
  path
  attributes <- attributes(path)

  out <- load_alldata(path, sheet = sheet)

  if (!is.null(cluster)) {
    dir    <- cluster$dir
    pos    <- cluster$pos
    regex  <- cluster$regex
    offset <- cluster$offset
    dim    <- cluster$dim
    if (dir == "row") {
      out <- extract_clusters(df = out, regex = regex, col = pos,
                            offset = offset, dim = dim)
    } else if (dir == "col") {
      out <- extract_clusters(df = out, regex = regex, row = pos,
                            offset = offset, dim = dim)
    } else {
      stop("Unknown dir")
    }
  out
  }

  if (attributes$row_merged > 0) {
    out <- unmerge_vert(out, col = attributes$row_merged)
  }

  if (attributes$col_merged > 0) {
    out <- unmerge_horiz(out, row = attributes$col_merged) %>%
      merge_colname(rows = 1:(attributes$col_merged + 1))
  }
  if (is.list(out) & is.null(dim(out))) {
    out <- out %>%
      lapply(headerize, row = 1) %>%
      purrr::invoke(rbind, .)
  } else {
    out <- headerize(as.data.frame(out), row = 1) %>% tibble::as_tibble()
  }
}

#' Rebel against godly Excel workbook
#'
#' @inheritParams rebel_sheet
#' @param sheet.regex Regular expression to match sheetname
rebel <- function(path, sheet.regex, row_merged = 0, col_merged = 0,
                  cluster = NULL) {
  sheets <- stringr::str_extract(readxl::excel_sheets(path), sheet.regex) %>%
    stats::na.omit()
  out <- purrr::map_df(sheets, rebel_sheet, path = path,
             row_merged = row_merged,
             col_merged = col_merged,
             cluster = cluster)
  out
  as.data.frame(out)
}
