#' Replace NAs in given vector by the repetition of the prior value
#'
#' @param x Vector containing value and NA
rep_na_rep <- function(x) {
  out  <- x
  fill <- NA
  for (i in seq_along(x)) {
    if (!is.na(x[i])) {
      fill <- x[i]
    }
    out[i] <- fill
  }
  out
}

#' Paste characters in multiple rows on the given columnn
#'
#' @param col Position of the target column
#' @param rows Rows to be concatenated
#' @inheritParams make_rect
paste_rows <- function(col, rows, df) {
  df[rows, ] %>%
  dplyr::pull(col) %>%
  paste0(collapse = "_")
}

#' Make vector \emph{houganshi}
#'
#' This function returns vector \emph{houganshi} to locate the position of
#'   target word (e.g. species name) in a Excel rows or columns.
#' To keep correspondence between nubmer of cells and nchar of output string,
#' This function replaces \code{NA} and a cell value with multiple characters.
#' @param str String vector with NA or multiple characters
#' @return Long single string composed of single-word cell and whitespace
#' @examples
#' \dontrun{
#'   str <- rep(1:10, 10) %>%
#'     replace(which(. %% 3  == 0), NA) %>%
#'     replace(which(. %% 5  == 0), "foo") %>%
#'     as.character()
#'   make_hougan(str)
#' }
make_hougan <- function(str) {
  out <- tidyr::replace_na(str, " ")
  out[nchar(out) != 1] <- " "
  out %<>% stringr::str_c(collapse = "")
  out
}

#' Expand single value list to data frame
#'
#' @param list List with `key = value` pairs
#' @param nrow Nrows of df to be created
list2df <- function(list, nrow) {
  names <- names(list)
  list %>%
    unlist() %>%
    unname() %>%
    rep(nrow, each = nrow) %>%
    matrix(ncol = length(list), nrow = nrow) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    magrittr::set_colnames(names)
}

#' Locate keywords in row or column of the given data frame
#'
#' @inheritParams make_rect
#' @param row Row position of df where the keyword appears
#' @param col Column position of df where the keyword appears
#' @param regex Regex to match keyword
locate_keys <- function(df, row = NULL, col = NULL, regex){
  if ( (!is.null(row) & !is.null(col)) |
      (is.null(row) & is.null(col))) {
    stop("Give either 'row' or 'col'")
  } else if (!is.null(row)){
    str <- df[row, ]
  } else if (!is.null(col)){
    str <- df[, col]
  } else {
    stop("Unknown case")
  }
  stringr::str_which(str, regex)
}

#' Return the location of decrese in given vector
#'
#' @param x Numeric vector to be scanned
which_decrease <- function(x) {
  if (any(diff(x) < 0)) {
    message("There is a decrease in given vector.")
    out <- which(diff(x) < 0)
    out <- structure(out, is.decrease = TRUE)
    return(out)
  } else {
    x <- structure(x, is.decrease = FALSE)
    x
  }
}

#' Alert if skip detected in given vector
#'
#' @inheritParams which_decrease
alert_skip <- function(x) {
  if (any(abs(diff(x)) > 1)) {
    stop("There is a skip in given vector")
  } else {
    x
  }
}

#' Convert vectors composed only numeric jpyear without names of the eras.
#'
#' @param x numeric jpyear vectors
#' @param start Name of the era
jpyr2ad <- function(x, start) {
  conv <- vector(mode = "integer") # To store conversion coefficients
  suppressMessages(era_changed <- attributes(which_decrease(x))$is.decrease)
  if (start == "showa") {
    if (era_changed) {
      pos_lastyr <- which_decrease(x)
      conv[(pos_lastyr + 1):length(x)] <- 1988
    } else {
      pos_lastyr <- length(x)
    }
    conv[1:pos_lastyr] <- 1925
  } else {
    stop("jpyr2ad")
  }
  ad <- x + conv[1:length(x)]
  alert_skip(ad)
  ad
}
