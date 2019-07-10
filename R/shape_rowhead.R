#' Remove NA from vector
rm_na <- function(x) {
  x[!is.na(x)]
}

#' Remove space from vector
rm_space <- function(x) {
  stringr::str_remove_all(x, "\\s")
}

#' Itemize table 
itemize <- function(tbl, only_item = FALSE) {
  itemize_ <- function(row, tbl) {
    vec2list <- function(vec) {
      vec <- vec %>%
        rev() %>%
        rm_na() %>%
        rm_space()
      list <- vector("list", length(vec))
      for (i in seq_along(vec)) {
        list[[i]] <- vec[i]
      }
      lvec <- length(vec)
      if (lvec > 1) {
        assertthat::assert_that(length(list) == lvec,
                                msg = "nrow differs between df and reference")
        names(list) <- c("item", paste0("group", 1:(length(vec) - 1)))
      } else {
        names(list) <- "item"
      }
      list
    }
    tbl %>%
       vectorize_row(row) %>%
       vec2list()
  }
  out <- purrr::map_df(1:nrow(tbl), itemize_, tbl = tbl)
  if (only_item == TRUE) return(out$item)
  out
}

tidy_rowhead <- function(df, range_header, into) {
  bad <- df[, range_header] %>%
    itemize(only_item = TRUE)
  body <- df[, -range_header]

  assertthat::assert_that(all(into$item == bad),
                          msg = "Row header changed")
  tibble::as_tibble(cbind(into[, seq(dim(into)[2],1)], body))
}
