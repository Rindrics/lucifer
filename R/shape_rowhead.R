#' Remove NA from vector
rm_na <- function(x) {
  x[!is.na(x)]
}

#' Remove space from vector
rm_space <- function(x) {
  stringr::str_remove_all(x, "\\s")
}

#' Itemize table 
itemize <- function(tbl, header_of, only_item = FALSE) {
  itemize_ <- function(row, tbl) {
    vec2list <- function(vec) {
      if (all(is.na(vec))) return(list(item = ""))
      vec <- vec %>%
        rm_na() %>%
        rm_space()
      olist <- vector("list", length(vec))
      for (i in seq_along(vec)) {
        olist[[i]] <- vec[i]
      }
      lvec <- length(vec)
      if (lvec > 1) {
        assertthat::assert_that(length(olist) == lvec,
                                msg = "nrow differs between df and reference")
        names(olist) <- c(paste0("group", 1:(length(vec) - 1)), "item")
      } else {
        names(olist) <- "item"
      }
      olist
    }
    tbl %>%
      vectorize_row(row) %>%
      vec2list()
  }
  if (header_of == "col") tbl <- t(tbl)
  out <- purrr::map_df(1:nrow(tbl), itemize_, tbl = tbl)
  if (only_item == TRUE) return(out$item)
  out
}

tidy_header <- function(df, type, range, ideal = NULL) {
  if (type == "row") {
    header <- df[, range] %>%
      itemize(header_of = "row") %>%
      as.matrix()
    body   <- df[, -(1:max(range))] %>%
      as.matrix()
    if (is.null(ideal)) {
      tibble::as_tibble(cbind(header[, seq(dim(header)[2],1)], body))
    } else {
      header <- header[1:nrow(ideal), ]
      body   <- body[1:nrow(ideal), ]
      assertthat::assert_that(all(ideal$item == header[, "item"]),
                              msg = "Row header changed")
      tibble::as_tibble(cbind(ideal[, seq(dim(ideal)[2], 1)], body))
    }
  } else {
    header <- df[range, ] %>%
      itemize(header_of = "col") %>%
      as.matrix()
    body   <- df[-(1:max(range)), ] %>%
      as.matrix()
    if (is.null(ideal)) {
      tibble::as_tibble(rbind(t(header[seq(1, dim(header)[1]), ]), body))
    } else {
      header <- header[, 1:ncol(ideal)]
      body   <- body[, 1:ncol(ideal)]
      assertthat::assert_that(all(ideal$item == header[, "item"]),
                              msg = "Column header changed")
      tibble::as_tibble(rbind(ideal[seq(1, dim(ideal)[1]), ], body))
    }
  }
}
