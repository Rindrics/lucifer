return_match <- function(x, regex) {
  match <- x %>%
    stringr::str_match(regex)
  out <- match[,2] %>%         # '2' points $1 regex capture
    na.omit() %>% as.vector()
  out
}

filter_sheet <- function(x) {
  re_omit  <- "^(?!.*0000)(?!体長)(?!Sheet)"
  re_match <- "((?:0[1-9][0-9]{2}|1[0-2][0-9]{2}).*)"
  regex <- paste0(re_omit, re_match)
  out   <- return_match(x, regex)
  out
}

parse_year <- function(x) {
  regex <- "^鮮魚測定([0-9]{2})$"
  match <- return_match(x, regex)
  if (length(match) == 0) {
    out <- NA
  } else {
    out <- paste0(20, match)
  }
  out
}

parse_date <- function(year, mmdd) {
  mm  <- substr(mmdd, 1, 2)
  dd  <- substr(mmdd, 3, 4)
  out <- paste(year, mm, dd, sep = "-")
  out
}

get_spcsname <- function(x) {
  regex <- "(カタクチ|マ(?!アジ)|ウルメ|マアジ)"
  match <- return_match(x, regex)
  if (length(match) == 0) {
    out <- NA
  } else {
    switch(match,
           "マアジ" = out <- match,
           out <- paste0(match, "イワシ"))
  }
  out
}

parse_str <- function(x, type) {
  switch(type,
         "sheet" = out <- filter_sheet(x),
         "year"  = out <- parse_year(x),
         "fish"  = out <- get_spcsname(x),
         stop("unknown case"))
  out
}
