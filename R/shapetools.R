#' Make loaded df as rectangular-shape df
#'
#' @inheritParams readxl::read_excel
#' @param df Data frame to be processed
#' @param range Cell range to be extracted in Excel-format("A1:Z10")
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
#' @inheritParams make_rect
#' @export
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
#' @inheritParams make_rect
#' @export
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
#' @inheritParams make_rect
#' @export
rm_sumrow <- function(df, key, colname) {
  colpos <- stringr::str_which(df[1, ], colname)
  target <- dplyr::pull(df, colpos)
  df[-stringr::str_which(target, key), ]
}

#' Remove summary cols from df
#'
#' @param key Regex to detect summary cols
#' @param rowname Name of the row contains key
#' @param regex If TRUE, rowname was recognized by regular expression
#' @inheritParams make_rect
#' @export
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

#' Merge colnames of multiple rows
#'
#' @param rows Rows of the target colnames to be concatenated
#' @param cols Numbers of target columns if given
#' @inheritParams make_rect
#' @export
merge_colname <- function(df, rows, cols = NULL) {
  cname   <- df[rows[1], ]
  nocname <- df[-rows, ]
  if (is.null(cols)) {
    cols <- 1:ncol(df)
  }
  cname[cols] <- purrr::map(cols, paste_rows, rows, df) %>%
  stringr::str_remove_all("_\\s|_NA")
  rbind(cname, nocname)
}

#' Convert full-width numbers in df into ASCII numbers
#'
#' @inheritParams make_rect
#' @param col Number of the target column
#' @param numerize If TRUE, remove characters convert column to numeric
#' @export
make_ascii <- function(df, col, numerize = FALSE) {
  ascii <- df %>%
    dplyr::pull(col) %>%
    purrr::map_chr(Nippon::zen2han)
  if (numerize) {
    df[, col] <- ascii %>%
      stringr::str_remove_all("\\D") %>%
      readr::parse_number()
  } else {
    df[, col] <- ascii
  }
  df
}

#' Change specific row into df header
#'
#' @inheritParams make_rect
#' @param row Position of the row to make df header
#' @export
headerize <- function(df, row) {
  body <- df[-row, ]
  head <- df[row, ]
  magrittr::set_colnames(body, head)
}

#' Extract a block from df using the keyword
#'
#' This function is the substancial function of \code{extract_blocks()}.
#' @inheritParams make_rect
#' @param direction The direction to which data blocks distribute
#' @param find_from The row or column position
#'   which \code{excract_block()} search key
#' @param pos.key Position where the \code{regex} of \code{extract_blocks()}
#'   matched the keyword
#' @param offset The offset (\code{c(row, pos})) of the block topleft from
#'   the coordination of keyword
#' @param dim Dimension (\code{c(row, col)}) of the block
extract_a_block <- function(pos.key, find_from, direction, df,
                          offset = c(0, 0), dim) {
  rofst <- offset[1]
  cofst <- offset[2]
  nrow  <- dim[1]
  ncol  <- dim[2]
  if (direction == "row") {
    row <- pos.key + rofst
    col <- find_from + cofst
  } else {
    row <- find_from + rofst
    col <- pos.key + cofst
  }
  df[row:(row + nrow - 1), col:(col + ncol - 1)]
}
#' Extract data blocks from data frame using the keyword
#'
#' @inheritParams make_rect
#' @inheritParams extract_a_block
#' @param regex Regular expression to match keywords
#' @param col Column position from which the keyword to be searched
#' @param row Row position from which the keyword to be searched
extract_blocks <- function(df, regex, col = NULL, row = NULL,
                           offset = c(0, 0), dim) {
  if (!is.null(row)) {
    pos <- locate_keys(df = df, row = row, regex = regex)
    purrr::map(pos, extract_a_block, find_from = row,
               direction = "col", df = df, offset = offset, dim = dim)
  } else if (!is.null(col)) {
    pos <- locate_keys(df = df, col = col, regex = regex)
    purrr::map(pos, extract_a_block, find_from = col,
               direction = "row", df = df, offset = offset, dim = dim)
  } else {
    stop("Unknown case")
  }
}

#' Gather month columns to rows
#'
#' @param df Data frame with month column
mcol2row <- function(df) {
  rtype <- attributes(df)$row.type
  if (!is.null(rtype)) {
    if (rtype == "jY") {
      out <- df %>%
        dplyr::mutate(year = jpyr2ad(year, "showa"))
    } else if (rtype %in% c("fisY", "Y")){
      out <- df
    } else {
      stop("Unknown row.type")
    }
  } else {
    message("No row.type in df.")
  }
  out <- out %>%
    dplyr::mutate(rowname = 1:nrow(df)) %>% #To resort after tidyr::gather()
    tidyr::gather(key = month, value = catch,
                  tidyselect::matches("[0-9][0-9]?")) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(month = as.integer(month)) %>%
    dplyr::arrange(rowname) %>%
    dplyr::select(-rowname)
  if (rtype == "fisY") {
    out %>%
      dplyr::mutate(year = ifelse(dplyr::between(month, 1, 3),
                                  year + 1,
                                  year)) %>%
      dplyr::arrange(year)
  } else {
    out
  }
}
