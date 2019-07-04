#' Rebel against godly Excel worksheet
#'
#' @inheritParams readxl::read_excel
#' @param row_merged Row position of merged colnames
#' @param col_merged Column position of merged rownames
#' @param cluster List of parameters to control \code{\link{unclusterize}}.
#'  \describe{
#'    \item{dir}{direction of the cluster evolution either of
#'    \code{"h"} (horizontal) or \code{"v"} (vertical)}
#'    \item{pos}{row- or column- position of the key to locate the cluster}
#'    \item{regex}{same as that of \code{\link{unclusterize}}}
#'    \item{offset}{same as that of \code{\link{unclusterize}}}
#'    \item{dim}{same as that of \code{\link{unclusterize}}}
#'    \item{info}{same as that of \code{\link{unclusterize}}}
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
#' @param col_type List of parameters to control \code{\link{gather_cols}}
#' @param col_omit List of parameters to control \code{\link{rm_matchcol}}
#' @param row_omit List of parameters to control \code{\link{rm_matchrow}}
#' @param unfiscalize List of parameters to control \code{\link{unfiscalize}}
#' @export
rebel_sheet <- function(sheet, path, row_merged = 0, col_merged = 0,
                        cluster = NULL, row_type = NULL, col_type = NULL,
                        row_omit = NULL, col_omit = NULL,
                        unfiscalize = c(month_start = NULL, rule = NULL)) {

  out <- load_alldata(path, sheet = sheet)

  if (!is.null(cluster)) {
    out <- unclusterize(df = out, regex = cluster$regex, direction = cluster$dir,
                        pos = cluster$pos, offset = cluster$offset,
                        ends = cluster$ends, info = cluster$info)
    if (cluster$dir == "v") {
      out <- lapply(out, make_ascii, row = cluster$pos)
    } else if (cluster$dir == "h") {
      out <- lapply(out, make_ascii, col = cluster$pos)
    }
  }

  if (row_merged > 0) {
    out <- unmerge_vert(out, col = row_merged)
  }

  if (col_merged > 0) {
    out <- unmerge_horiz(out, row = col_merged) %>%
      merge_colname(rows = 1:(col_merged + 1))
  }

  if (!is.null(row_omit)) {
    out <- rm_matchrow(out,
                     key = row_omit$key,
                     colpos = row_omit$colpos,
                     regex = row_omit$regex)
  }

  if (!is.null(col_omit)) {
    out <- rm_matchcol(out,
                     key = col_omit$key,
                     rowpos = col_omit$rowpos,
                     regex = col_omit$regex)
  }

  if (is.list(out) & is.null(dim(out))) {
    out <- out %>%
      lapply(headerize, row = 1) %>%
      purrr::invoke(rbind, .) %>%
      rm_nacols() %>%
      add_reference(path, sheet)
  } else {
    out <- headerize(as.data.frame(out), row = 1) %>%
      rm_nacols() %>%
      tibble::as_tibble() %>%
      add_reference(path, sheet)
  }

  if (!is.null(col_type)) {
    out <- gather_cols(df = out,
                       regex = col_type$regex,
                       newname = col_type$newname,
                       varname = col_type$varname)
    if (col_type$newname == "month") {
      out <- dplyr::mutate(out, month = stringr::str_remove(month, "\\D") %>%
                             as.integer())
    }
  }

  if (!is.null(row_type)) {
    if (row_type == "Y") {
      colnames(out)[1] <- "year"
      out <- dplyr::mutate(out, year = as.integer(year))
    }
    if (row_type == "fisY") {
      colnames(out)[1] <- "fisy"
      if (is.null(unfiscalize["month_start"]) ||
          is.null(unfiscalize["month"])) {
        rlang::abort(message = "Use 'unfiscalize = c(month_start =, rule =)'",
                     .subclass = "rebel_error",
                     unfiscalize = unfiscalize)
      } else {
        pos_monthcol <- stringr::str_which(colnames(out), "month")
        out <- unfiscalize(out, ycol = 1, mcol = pos_monthcol,
                           month_start = as.integer(unfiscalize["month_start"]),
                           rule = unfiscalize["rule"])
      }
    }
  }
  tibble::as_tibble(out)
}

#' Rebel against godly Excel workbook
#'
#' @inheritParams rebel_sheet
#' @param sheet_regex Regular expression to match sheetname
#' @export
rebel <- function(path, sheet_regex, row_merged = 0, col_merged = 0,
                  cluster = NULL, row_type = NULL, col_type = NULL,
                  row_omit = NULL, col_omit = NULL, 
                  unfiscalize = c(month_start = NULL, rule = NULL)) {
  sheets <- stringr::str_extract(readxl::excel_sheets(path), sheet_regex) %>%
    stats::na.omit()
  out <- purrr::map_df(sheets, rebel_sheet, path = path,
                       row_merged = row_merged,
                       col_merged = col_merged,
                       cluster = cluster,
                       row_type = row_type,
                       col_type = col_type,
                       row_omit = row_omit, col_omit = col_omit,
                       unfiscalize)
  tibble::as_tibble(out)
}
