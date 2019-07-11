#' Extract a cluster from df using the keyword
#'
#' This function is the substancial function of \code{unclusterize}.
#' @inheritParams make_rect
#' @param direction The direction to which data clusters distribute
#' @param find_from The row or column position
#'   which \code{excract_cluster()} search key
#' @param pos_key Position where the \code{regex} of \code{unclusterize}
#'   matched the keyword
#' @param offset The offset (\code{c(row, pos})) of the cluster topleft from
#'   the coordination of keyword
#' @param ends List of regex to locate row- and column- ends of each cluster
#'   Form should be like \code{ends = list(row = "2019", col = "[Dd]ecember$")}.
#' Regex \code{row = } must specify the end of 'left most' columnn of df,
#'  not that of the column with key matched by \code{regex}
#' @param info Parameters to make key:value list such as
#' \describe{
#'  \item{key_offset}{Offset \code{c(row, col)} of \code{key} topleft
#'   from df topleft. If \code{NULL}, automatically set to \code{keyn}}
#'  \item{key_dim}{Dimension \code{c(row, col)} of \code{key}}
#'  \item{value_offset}{Offset \code{c(row, col)} of \code{value} topleft from
#'   df topleft}
#'  \item{value_dim}{Dimension \code{c(row, col)} of \code{value}}
#' }
extract_a_cluster <- function(pos_key, find_from, direction, df,
                              offset = c(0, 0), ends, info = NULL) {
  rofst <- offset[1]
  cofst <- offset[2]

  if (direction == "row") {
    row <- pos_key + rofst
    col <- find_from + cofst
    maxrow <- locate_matchend(dplyr::pull(df, col)[row:nrow(df)],
                              ends[["row"]]) + row - 1
    maxcol <- locate_matchend(vectorize_row(df, row), ends[["col"]])
    nrow <- maxrow - pos_key - rofst + 1
    ncol <- maxcol - cofst - (find_from - 1)
  } else {
    row <- find_from + rofst
    col <- pos_key + cofst
    maxrow <- locate_matchend(dplyr::pull(df, col), ends[["row"]])
    maxcol <- locate_matchend(vectorize_row(df, row)[col:ncol(df)],
                              ends[["col"]]) + col - 1
    nrow <- maxrow - rofst - (find_from - 1)
    ncol <- maxcol - pos_key - cofst + 1
  }

  out <- df[row:(row + nrow - 1), col:(col + ncol - 1)]

  if (offset[1] == -1 && offset[2] == 0) {
    out[1, 1] <- out[2, 1]
    out <- out[-2, ]
  }

  if (is.null(info)) return(out)

  value_offset <- info$value_offset
  value_dim    <- info$value_dim
  rvalue       <- row + value_offset[1]
  cvalue       <- col + value_offset[2]
  value        <- df[rvalue:(rvalue + value_dim[1] - 1),
                     cvalue:(cvalue + value_dim[2] - 1)] %>%
    unlist() %>%
    as.vector()
  if (value_offset[1] > 0) out <- out[- (value_offset[1] + 1), ]
  if (is.null(info$key_offset)) {
    key <- paste0("key", 1:max(value_dim))
  } else {
    key_offset   <- info$key_offset
    key_dim      <- info$key_dim
    rkey <- row + key_offset[1]
    ckey <- col + key_offset[2]
    key  <- df[rkey:(rkey + key_dim[1] - 1),
               ckey:(ckey + key_dim[2] - 1)] %>%
      unlist() %>% as.vector()
  }

  info_list <- as.list(stats::setNames(value, key))
  out %>%
    append_info(info = info_list, headerized = FALSE)
}

#' Extract data clusters from data frame using the keyword
#'
#' This function extracts data clusters from single Excel sheet.
#' @inheritParams make_rect
#' @inheritParams extract_a_cluster
#' @param regex Regular expression to match keywords
#' @param direction Directoin of the cluster revolution
#' @param pos Positon of row/column to scan using \code{regex}
#' @export
unclusterize <- function(df, regex, direction, pos,
                         offset = c(0, 0), ends, info = NULL) {
  if (direction == "h") {
    pos_key <- locate_keys(df = df, row = pos, regex = regex)
    purrr::map(pos_key, extract_a_cluster, find_from = pos,
               direction = "col", df = df,
               offset = offset, ends = ends, info = info)
  } else if (direction == "v") {
    pos_key <- locate_keys(df = df, col = pos, regex = regex)
    purrr::map(pos_key, extract_a_cluster, find_from = pos,
               direction = "row", df = df,
               offset = offset, ends = ends, info = info)
  } else {
    warning("Set 'direction' correctly")
    return(df)
  }
}
