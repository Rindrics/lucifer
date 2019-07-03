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

#' Append information stored in list to data frame
#'
#' @param info List conposed of `key = value` pairs
#' @param headerized If FALSE, allow appending to data frame with
#'   tentative colnames
#' @inheritParams make_rect
append_info <- function(info, df, headerized = FALSE) {
  df_info <- list2df(info, nrow = nrow(df))
  if (headerized == FALSE) {
    df_info[1, ] <- names(info)
    tentative_name <- as.character(seq(ncol(df) + 1, ncol(df) + length(info)))
    colnames(df_info) <- tentative_name
  }
  cbind(df, df_info)
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

#' Gather columns to variable
#'
#' @inheritParams make_rect
#' @param regex Regex to match columns to be gathered
#' @param newname New name for colnames to be gatherd
#' @param varname New name for values to be gatherd
gather_cols <- function(df, regex, newname, varname) {
  cols2gather <- stringr::str_extract(colnames(df), regex) %>%
    stats::na.omit()
  df %>%
    tidyr::gather_(cols2gather, key = newname, value = varname)
}

#' Remove rows matched to key
#'
#' @param key Name of rows to be removed
#' @param colpos Position of the colmun contains key
#' @param regex If TRUE, \code{key} was recognized as regular expression
#' @inheritParams make_rect
#' @export
rm_matchrow <- function(df, key, colpos, regex) {
  df <- as.data.frame(df)
  target <- dplyr::pull(df, colpos)
  if (regex) {
    df[-stringr::str_which(target, key), ]
  } else {
    key_noregex <- paste0("^", key, "$")
    df[-stringr::str_which(target, key_noregex), ]
  }
}

#' Remove columns matched to key
#'
#' @param key Name of columns to be removed
#' @param rowpos Position of the row contains \code{key}
#' @param regex If TRUE, \code{key} was recognized as regular expression
#' @inheritParams make_rect
#' @export
rm_matchcol <- function(df, key, rowpos, regex) {
  target <- dplyr::slice(df, rowpos) %>%
    unlist() %>%
    as.vector()
  if (regex) {
    df[, -stringr::str_which(target, key)]
  } else {
    key_noregex <- paste0("^", key, "$")
    df[, -stringr::str_which(target, pattern = key_noregex)]
  }
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
#' @param row Number of the target row
#' @param numerize If TRUE, remove characters convert column to numeric
#' @param headerized If FALSE (default), allow df with tentative colnames
#' @export
make_ascii <- function(df, col = NULL, row = NULL,
                       numerize = FALSE, headerized = FALSE) {
  row_offset <- 0

  if (headerized) {
    header <- colnames(df)
    body   <- df
  } else {
    header     <- vectorize_row(df, 1)
    body       <- df[-1, ]
    row_offset <- -1
  }

  if (is.null(col) & is.null(row)) {
    rlang::abort(message = "Give me at least 'col' or 'row'.",
                 .subclass = "make_ascii_error")
  } else {
    edit_row    <- !is.null(row) && (row > 1 | headerized == TRUE)
    edit_col    <- !is.null(col)
    edit_header <- !is.null(row) && row == 1 && headerized == FALSE

    if (edit_col) {
      string <- dplyr::pull(body, col)
    } else if (edit_header) {
      string <- header
    } else if (edit_row) {
      string <- vectorize_row(body, row + row_offset)
    }

    ascii <- purrr::map_chr(string, Nippon::zen2han)

    if (numerize) {
      ascii <- ascii %>%
        stringr::str_remove_all("\\D")
    }

    if (edit_col) {
      body[, col] <- ascii
    } else if (edit_header) {
      header <- ascii
    } else if (edit_row) {
      body[row + row_offset, ] <- ascii
    }
    if (headerized) {
      colnames(body) <- header
      out <- body
    } else {
      out <- rbind(header, body)
    }
    out
  }
}

#' Change specific row into df header
#'
#' @inheritParams make_rect
#' @param row Position of the row to make df header
#' @export
headerize <- function(df, row) {
  df   <- as.data.frame(df)
  body <- df[-row, ]
  head <- df[row, ] %>%
    as.character() %>%
    make.unique()
  magrittr::set_colnames(body, head)
}

#' Remove column whose colname is NA
#'
#' @inheritParams make_rect
#' @export
rm_nacols <- function(df) {
  df_leftmost <- df[, 1]
  df_right    <- df[, -1]
  name_left   <- colnames(df)[1]
  name_right  <- colnames(df)[-1]
  not_na <- !stringr::str_detect(colnames(df_right), "^NA(.[1-9]+)?$")
  if (length(not_na) == 0) {
    df
  } else {
    not_na <- tidyr::replace_na(not_na, FALSE)
    not_na
    out <- cbind(df_leftmost, df_right[, not_na]) %>%
      data.frame(stringsAsFactors = FALSE)
    colnames(out) <- c(name_left, name_right[not_na])
    out[, 1] <- as.character(out[, 1])
    out
  }
}

#' Extract a cluster from df using the keyword
#'
#' This function is the substancial function of \code{extract_clusters()}.
#' @inheritParams make_rect
#' @param direction The direction to which data clusters distribute
#' @param find_from The row or column position
#'   which \code{excract_cluster()} search key
#' @param pos.key Position where the \code{regex} of \code{extract_clusters()}
#'   matched the keyword
#' @param offset The offset (\code{c(row, pos})) of the cluster topleft from
#'   the coordination of keyword
#' @param ends List of regex to locate row- and column- ends of each cluster
#'   Form should be like \code{ends = list(row = "2019", col = "[Dd]ecember$")}
#' @param info Parameters to control \code{link{append_info}}
#' @export
extract_a_cluster <- function(pos.key, find_from, direction, df,
                              offset = c(0, 0), ends, info = NULL) {
  rofst <- offset[1]
  cofst <- offset[2]

  if (direction == "row") {
    row <- pos.key + rofst
    col <- find_from + cofst
    maxrow <- locate_matchend(dplyr::pull(df, col)[row:nrow(df)],
                              ends[["row"]]) + row - 1
    maxcol <- locate_matchend(vectorize_row(df, row), ends[["col"]])
    nrow <- maxrow - pos.key - rofst + 1
    ncol <- maxcol - cofst
    nrow
    ncol
  } else {
    row <- find_from + rofst
    col <- pos.key + cofst
    maxrow <- locate_matchend(dplyr::pull(df, col), ends[["row"]])
    maxcol <- locate_matchend(vectorize_row(df, row)[col:ncol(df)],
                              ends[["col"]]) + col - 1
    nrow <- maxrow - rofst
    ncol <- maxcol - pos.key - cofst + 1
  }

  out <- df[row:(row + nrow - 1), col:(col + ncol - 1)]

  if (offset[1] == -1 && offset[2] == 0) {
    out[1, 1] <- out[2, 1]
    out <- out[-2, ]
    head(out)
  }
  if (!is.null(info)) {
    row_info  <- row + info$offset[1]
    col_info  <- col + info$offset[2]
    nrow_info <- info$dim[1]
    ncol_info <- info$dim[2]
    infos     <- df[row_info:(row_info + nrow_info - 1),
                    col_info:(col_info + ncol_info - 1)]
    info_list <- as.list(stats::setNames(infos[[2]], infos[[1]]))
    out       <- append_info(info = info_list, df = out, headerized = FALSE)
  }
  out
}

#' Extract data clusters from data frame using the keyword
#'
#' This function extracts data clusters from single Excel sheet.
#' @inheritParams make_rect
#' @inheritParams extract_a_cluster
#' @param regex Regular expression to match keywords
#' @param col Column position from which the keyword to be searched
#' @param row Row position from which the keyword to be searched
#' @export
extract_clusters <- function(df, regex, col = NULL, row = NULL,
                             offset = c(0, 0), ends, info = NULL) {
  if (!is.null(row)) {
    pos <- locate_keys(df = df, row = row, regex = regex)
    purrr::map(pos, extract_a_cluster, find_from = row,
               direction = "col", df = df,
               offset = offset, ends = ends, info = info)
  } else if (!is.null(col)) {
    pos <- locate_keys(df = df, col = col, regex = regex)
    purrr::map(pos, extract_a_cluster, find_from = col,
               direction = "row", df = df,
               offset = offset, ends = ends, info = info)
  } else {
    stop("Unknown case")
  }
}

#' Convert sheetname to variable
#'
#' @inheritParams make_rect
#' @param as Name of the new column which contains sheetnames
sheet2var <- function(df, as) {
  sheetname <- attr(df, "sheetname")
  out <- df %>%
    dplyr::mutate(!! as := sheetname)
}

#' Merge data from different sheets into single df
