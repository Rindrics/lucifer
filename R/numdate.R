# This file is tangled from tinyplyr.org.
# (https://github.com/smxshxishxad/tinyplyr/tinyplyr.org)
# Edit that file.

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

jpdate2juliani <- function(x) {
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
    jday <- julian.Date(date, origin = as.Date("1900-01-01"))
    jday <- jday + 1 # Correct origin difference
    jday <- jday + 1 # Correct leap year bug
  } else {
    jday <- x
  }
  as.numeric(jday[1])
}

#' Convert Japanese Calendar date to Excel Julian day
#'
#' @param x Date string in Japanes Calendar format.
#' @return Julian day (Microsoft Excel style: origin = 1900-01-01).
#' @examples
#' jpdate2julian("H30.01.01")
#' jpdate2julian("H30-01-01")
#' jpdate2julian("H30.1.1")
#' jpdate2julian("H30-1-1")
#' @export
jpdate2julian <- function(x) {
  out <- purrr::map(x, jpdate2juliani)
  out <- as.vector(unlist(out))
  out
}
