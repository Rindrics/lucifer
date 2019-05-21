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
