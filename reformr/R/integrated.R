make_outfname <- function(fpath) {
  dirvec <- stringr::str_split(fpath, "/") %>%
    unlist()
  year     <- purrr::map2(dirvec, "year", parse_str) %>%
    unlist() %>% na.omit()
  spcsname <- purrr::map2(dirvec, "fish", parse_str) %>%
    unlist() %>% na.omit()
  sheetname <- readxl::excel_sheets(fpath) %>%
    filter_sheet()
  mmdd      <- filter_sheet(sheetname)
  out   <- paste0(year, "_", spcsname, "_", mmdd, ".csv")
  out
}
