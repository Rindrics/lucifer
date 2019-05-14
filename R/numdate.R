num2datei <- function(x) {
  jday_march01 <- 60
  x            <- as.numeric(x)
  if (x == jday_march01) {
    stop("This date is not correct in Excel.")
  }
  if (x > jday_march01) {
    x <- x - 1 # because of leap year
  }
  x    <- x - 1 # because of origin
  date <- as.character(as.Date(x, origin = "1900-01-01"))
  date
}

#' Get format of given date string
#'
#' @param x Date string to be judged
#' @param year \%Y of date string
#' @return Format of date string, one of '\%Y\%m\%d', '\%m\%d', or 'XLjday'.
get_datefmt <- function(x, year) {
  format <- NA
  if (stringr::str_detect(x, "^H\\.?\\d"))
    format <- "heisei"
  else if (nchar(x) == 4)
    format <- "%m%d"
  else if (nchar(x) == 8 && substr(x, 1, 4) == as.character(year))
    format <- "%Y%m%d"
  else if (lucifer::num2date(x) %>% substr(1, 4) == as.character(year))
    format <- "XLjday"
  else
    stop("Something's wrong with \"date\" data.")
  format
}

#' Split date str in japanese date format
#'
#' @param x Date string with year in Japanese era
#' @examples
#' split_jpdate("H30.01.01")
#' split_jpdate("H.30.01.01")
#' @export
split_jpdate <- function(x) {
  initial <- substr(x, 1, 1)
  switch(initial,
       "H" = era <- "heisei")

  year  <- stringr::str_match(x, "^[A-Za-z]\\.?([0-9]+)(?:\\.|-)")[2]
  month <- stringr::str_match(x, "^[A-Za-z]\\.?[0-9]+(?:\\.|-)([0-9]+)")[2]
  day   <-
    stringr::str_match(x,
                       "^[A-Za-z]\\.?[0-9]+(?:\\.|-)[0-9]+(?:\\.|-)([0-9]+)"
                       )[2]
  out   <- list("era"   = era,
                "year"  = as.numeric(year),
                "month" = as.numeric(month),
                "day"   = as.numeric(day)
                )
  out
}

#' Standardize date string in format "\%Y-\%m-\%d" format
#'
#' @inheritParams get_datefmt
#' @return Date string standardized as "\%Y-\%m-\%d" format.
#' @export
stdz_date <- function(x, year) {
  format <- get_datefmt(x, year)
  switch(format,
         "heisei" = {
           split  <- tinyplyr::split_jpdate(x)
           year <- paste0(split$era, split$year, "年") %>%
             Nippon::wareki2AD()
           date <-
             lubridate::ymd(paste(year, split$month, split$day, sep = "-"))
         },
         "%Y%m%d" = {
           yyyymmdd <- x
           date     <- lubridate::ymd(yyyymmdd)
         },
         "%m%d"   = {
           yyyymmdd <- paste0(year, x)
           date     <- lubridate::ymd(yyyymmdd)
         },
         "XLjday" = {
           date <- tinyplyr::num2date(x)
         })
  as.character(date)
}

#' Convert Julian date (Microsoft Excel style: origin = 1900-01-01)
#'   to \%Y-\%m-\%d
#'
#' @param x Julian day read from Excel spredsheets.
#' @return Date character in %Y-%m-%d format.
#' @examples
#' num2date(43466)
#' num2date(c(43466:43468, "43469"))
#' @export
num2date <- function(x) {
  out <- purrr::map(x, num2datei)
  out <- as.vector(unlist(out))
  out
}

#' Judge whether date string is written in Japanese Calendar or not
#'
#' @param x Date string. Both '.' and '-' are allowed for separaters.
#' @return Bool.
#' @examples
#' is.jpdate("H31.1.1")
#' is.jpdate("H31.01.01")
#' is.jpdate("H31-01-01")
#' is.jpdate("H3100-01-01")
#' @export
is.jpdate <- function(x) {
  stringr::str_detect(x, "[A-Z]\\.?[0-9]+(\\..|-)")
}


date2juliani <- function(x) {
  if (is.jpdate(x) == TRUE) {
    split  <- split_jpdate(x)
    era    <- split$era
    switch(era,
           "heisei" = {
             year  <- split$year + 1988
             month <- split$month
             day   <- split$day
             date  <- as.Date(paste(year, month, day, sep = "-"))
           })
  } else {
    date <- gsub("\\.", "-", x) %>%
      as.Date()
  }
  jday <- julian.Date(date, origin = as.Date("1900-01-01"))
  jday <- jday + 1 # Correct origin difference
  jday <- jday + 1 # Correct leap year bug
  as.numeric(jday[1])
}

#' Convert date to Excel Julian day
#'
#' @param x Date string in Gregorian or Japanes Calendar format.
#' @return Julian day (Microsoft Excel style: origin = 1900-01-01).
#' @examples
#' date2julian("H30.01.01")
#' date2julian("H30-01-01")
#' date2julian("H30.1.1")
#' date2julian("H30-1-1")
#' @export
date2julian <- function(x) {
  out <- purrr::map(x, date2juliani)
  out <- as.vector(unlist(out))
  out
}
