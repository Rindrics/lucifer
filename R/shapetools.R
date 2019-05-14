#' Make loaded df as rectangular-shape df
#'
#' @inheritParams readxl::read_excel
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
