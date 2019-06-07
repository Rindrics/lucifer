#' Rebel against godly Excel worksheet
#'
#' @inheritParams readxl::read_excel
#' @param row_merged Row position of merged colnames
#' @param col_merged Column position of merged rownames
#' @param cluster List of parameters to control \code{\link{extract_clusters}}.
#'  \describe{
#'    \item{dir}{direction of the cluster evolution}
#'    \item{pos}{row- or column- position of the key to locate the cluster}
#'    \item{regex}{same as that of \code{\link{extract_clusters}}}
#'    \item{offset}{same as that of \code{\link{extract_clusters}}}
#'    \item{dim}{same as that of \code{\link{extract_clusters}}}
#'  }
#' @param row_type Type of row one of
#'  \describe{
#'   \item{Y}{year}
#'   \item{YM}{year-month}
#'   \item{jY}{Japanese year}
#'   \item{jYM}{Japanese year-month}
#'   \item{fisY}{fiscal year}
#'   \item{fisYM}{fiscal year-month}
#'   \item{fisjYM}{fiscal Japanese year-month}
#'  }
#' @param col_type list of parameters to control \code{\link{mcol2row}}.
#'  \describe{
#'   \item{varname}{new varname, same as \code{value} of
#'                   \code{\link[tidyr]{gather}}}
#'  }
#' @param col_omit list of parameters to control \code{\link{rm_sumcol}}
#' @param row_omit list of parameters to control \code{\link{rm_sumrow}}
#' @param fullwidth List of parameters to cotrol \code{\link{make_ascii}}
#' @export
rebel_sheet <- function(sheet, path, row_merged = 0, col_merged = 0,
                        cluster = NULL, row_type = NULL, col_type = NULL,
                        row_omit = NULL, col_omit = NULL, fullwidth = NULL) {
  path <- structure(path,
                    sheet = sheet,
                    row_merged = row_merged,
                    col_merged = col_merged,
                    cluster = cluster,
                    row_type = row_type,
                    col_type = col_type,
                    row_omit = row_omit,
                    col_omit = col_omit)
  path
  attributes <- attributes(path)

  out <- load_alldata(path, sheet = sheet)

  if (!is.null(fullwidth)) {
    out <- make_ascii(out, col = fullwidth$col, numerize = fullwidth$numerize)
  }

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

  if (!is.null(attributes$row_omit)) {
    out <- rm_sumrow(out,
                     key = attributes$row_omit$key,
                     colpos = attributes$row_omit$colpos,
                     regex = attributes$row_omit$regex)
  }

  if (!is.null(attributes$col_omit)) {
    out <- rm_sumcol(out,
                     key = attributes$col_omit$key,
                     rowpos = attributes$col_omit$rowpos,
                     regex = attributes$col_omit$regex)
  }

  if (is.list(out) & is.null(dim(out))) {
    out <- out %>%
      lapply(headerize, row = 1) %>%
      purrr::invoke(rbind, .)
  } else {
    out <- headerize(as.data.frame(out), row = 1) %>% tibble::as_tibble()
  }


  if (!is.null(attributes$row_type)) {
    out <- mcol2row(structure(out, row_type = attributes$row_type),
                    varname = attributes$col_type$name)
  }
  out
}

#' Rebel against godly Excel workbook
#'
#' @inheritParams rebel_sheet
#' @param sheet_regex Regular expression to match sheetname
#' @export
rebel <- function(path, sheet_regex, row_merged = 0, col_merged = 0,
                  cluster = NULL, row_type = NULL, col_type = NULL,
                  row_omit = NULL, col_omit = NULL) {
  sheets <- stringr::str_extract(readxl::excel_sheets(path), sheet_regex) %>%
    stats::na.omit()
  out <- purrr::map_df(sheets, rebel_sheet, path = path,
             row_merged = row_merged,
             col_merged = col_merged,
             cluster = cluster,
             row_type = row_type,
             col_type = col_type,
             row_omit = row_omit, col_omit = col_omit)
  out
  as.data.frame(out)
}
