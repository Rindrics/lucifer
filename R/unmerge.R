#' Fill NAs of data frame by varname matched by regex
#'
#' @inheritParams make_rect
#' @param along \code{row} or \code{col}, along which NAs of df are filled up
#' @param pos Position of column or row to be processed
#' @export
fill_na <- function(df, along, pos) {
  df  <- as.data.frame(df)
  if (along == "v") {
    vars <- dplyr::pull(df, pos)
  } else {
    vars <- df[pos, ]
  }
  newvec <- stringr::str_match(vars, ".+") %>%
    rep_na_rep()
  out    <- df
  if (along == "v") {
    out[, pos] <- newvec
  } else {
    out[pos, ] <- newvec
  }
  as.data.frame(out)
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
