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
#' @export
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
  df         <- as.data.frame(df)
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
  df         <- as.data.frame(df)
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
#' @export
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
#' @param x Data frame or vector to be processed
#' @param col Number of the target column
#' @param row Number of the target row
#' @param numerize If TRUE, remove characters convert column to numeric
#' @param headerized If FALSE (default), allow df with tentative colnames
#' @export
make_ascii <- function(x, col = NULL, row = NULL,
                       numerize = FALSE, headerized = FALSE) {
  if (!is.data.frame(x) & !is.list(x)) {
    string <- x
  } else {
    row_offset <- 0
    x <- as.data.frame(x)
    if (headerized) {
      header <- colnames(x)
      body   <- x
    } else {
      header     <- vectorize_row(x, 1)
      body       <- x[-1, ]
      row_offset <- -1
    }
    if (is.null(col) & is.null(row)) {
      rlang::abort(message = "Give me at least 'col' or 'row'.",
                   .subclass = "make_ascii_error")
    } else {
      edit_row    <- !is.null(row) && (row > 1 | headerized == TRUE)
      edit_col    <- !is.null(col)
      edit_header <- !is.null(row) && row == 1 && headerized == FALSE
    }
    if (edit_col) {
      string <- dplyr::pull(body, col)
    } else if (edit_header) {
      string <- header
    } else if (edit_row) {
      string <- vectorize_row(body, row + row_offset)
    }
  }

  ascii <- purrr::map_chr(string, Nippon::zen2han)

  if (numerize) {
    ascii <- ascii %>%
      stringr::str_remove_all("\\D")
  }

  if (is.vector(x)) return(ascii)

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
rm_nacols <- function(df, except = NULL) {
  df_leftmost <- df[, 1]
  df_right    <- df[, -1]
  name_left   <- colnames(df)[1]
  name_right  <- colnames(df)[-1]
  not_na      <- !stringr::str_detect(colnames(df_right), "^NA(.[1-9]+)?$")
  if (length(not_na) == 0) {
    df
  } else {
    not_na <- c(tidyr::replace_na(not_na, FALSE), except)
    out    <- cbind(df_leftmost, df_right[, not_na]) %>%
      data.frame(stringsAsFactors = FALSE)
    colnames(out) <- c(name_left, name_right[not_na])
    out[, 1] <- as.character(out[, 1])
    out
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

#' Convert fiscal year column into true year
#'
#' @inheritParams make_rect
#' @inheritParams unfiscalize_vec
#' @param ycol Position of fiscal year column
#' @param mcol Position of month column
#' @export
unfiscalize <- function(df, ycol, mcol, month_start, rule) {
  df         <- as.data.frame(df)
  df[, ycol] <- as.integer(df[, ycol])
  df[, mcol] <- as.integer(df[, mcol])
  plist <- list(fisyr = df[, ycol],
                month = df[, mcol],
                month_start = month_start,
                rule = rule)
  trueyr <- purrr::pmap_int(plist, unfiscalize_vec)
  if (any(stringr::str_detect(colnames(df), "year"))) {
    df$trueyr <- trueyr
  } else {
    df$year   <- trueyr
  }
  tibble::as_tibble(df)
}
