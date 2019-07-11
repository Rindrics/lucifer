#' Rebel against godly Excel worksheet
#'
#' @inheritParams readxl::read_excel
#' @param col_header Row position of colnames extend across rows
#' @param row_header Column position of rownames extend accross colmns
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
rebel_sheet <- function(sheet, path,
                        col_header = NULL, row_header = NULL,
                        cluster = NULL, row_type = NULL, col_type = NULL,
                        row_omit = NULL, col_omit = NULL,
                        unfiscalize = c(month_start = NULL, rule = NULL),
                        ideal = NULL) {

  out <- load_alldata(path, sheet = sheet)

  if (!is.null(row_header) && length(row_header) > 1) {
    rows_colhead <- 1:max(col_header)
    upperside    <- out[rows_colhead, ]
    lowerside    <- out[-rows_colhead, ]
    out <- tidy_header(df = lowerside,
                       type = "row", range = row_header, ideal = ideal$row) %>%
      as.matrix() %>%
      rbind(as.matrix(upperside), .) %>%
      as.data.frame(stringsAsFactors = FALSE)
  }

  if (!is.null(col_header) && length(col_header) > 1) {
    cols_rowhead <- 1:max(row_header)
    rightside <- out[, -cols_rowhead]
    leftside  <- out[, cols_rowhead]
    out <- tidy_header(df = rightside,
                       type = "col", range = col_header, ideal = ideal$col) %>%
      as.matrix()
    leftside <- leftside[rev(seq(nrow(out), by = -1, length.out = nrow(out))), ]
    out <- cbind(as.matrix(leftside), out) %>%
      as.data.frame(stringsAsFactors = FALSE)
  }
  
  if (!is.null(col_header) && length(col_header) == 1) {
    out <- fill_na(out, along = "h", pos = col_header)
  }

  if (!is.null(row_header) && length(row_header) == 1) {
    out <- fill_na(out, along = "v", pos = row_header) %>%
      merge_colname(rows = 1:(row_header + 1))
  }

  if (is.null(cluster)) return(ceasefire(out, path, sheet, "cluster"))

  out <- unclusterize(df = out, regex = cluster$regex,
                      direction = cluster$dir,
                      pos = cluster$pos, offset = cluster$offset,
                      ends = cluster$ends, info = cluster$info)
  if (cluster$dir == "v") {
    out <- lapply(out, make_ascii, row = cluster$pos)
  } else if (cluster$dir == "h") {
    out <- lapply(out, make_ascii, col = cluster$pos)
  }

  if (!is.null(row_omit)) {
    out <- rm_matchrow(out,
                       key = row_omit$key,
                       colpos = row_omit$colpos,
                       regex = row_omit$regex)
  }

  if (!is.null(col_omit)) {
    out <- out %>%
      lapply(rm_matchcol, key = col_omit$key,
                       rowpos = col_omit$rowpos,
                       regex = col_omit$regex) %>%
      purrr::invoke(rbind, .)
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
rebel <- function(path, sheet_regex, col_header = NULL, row_header = NULL,
                  cluster = NULL, row_type = NULL, col_type = NULL,
                  row_omit = NULL, col_omit = NULL,
                  unfiscalize = c(month_start = NULL, rule = NULL),
                  ideal = NULL) {

  sheets <- stringr::str_extract(readxl::excel_sheets(path), sheet_regex) %>%
    stats::na.omit()

  out <- lapply(sheets, rebel_sheet, path = path,
                col_header = col_header, row_header = row_header,
                cluster = cluster, row_type = row_type, col_type = col_type,
                row_omit = row_omit, col_omit = col_omit, unfiscalize,
                ideal = ideal) %>%
          purrr::invoke(rbind, .)

    if (is.null(cluster)) return(ceasefire(out, funcname = "cluster"))
    tibble::as_tibble(out)
}
