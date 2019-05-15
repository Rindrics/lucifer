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
paste_rows <- function(col, rows, df) {
  df[rows, ] %>%
  dplyr::pull(col) %>%
  paste0(collapse = "_")
}
