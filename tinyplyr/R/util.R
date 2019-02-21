# ----------------------------------------------------------------------------.
# This file is tangled from tinyplyr.org.                                     |
# (https://github.com/smxshxishxad/tinyplyr/blob/master/tinyplyrjmdatar.org)  |
# Do not edit by hand.                                                        |
# ---------------------------------------------------------------------------'

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
}

#' @export
num2date <- function(x) {
  out <- purrr::map(x, num2datei)
  out <- as.vector(unlist(out))
}

#' @export
is.jpdate <- function(x) {
  stringr::str_detect(x, "[A-Z]\\.?[0-9]+\\..")
}

#' @export
split_jpdate <- function(x) {
  initial <- substr(x, 1, 1)
  switch(initial,
       "H" = era <- "heisei")

  year  <- stringr::str_match(x, "^[A-Za-z]\\.?([0-9]+)\\.")[2]
  month <- stringr::str_match(x, "^[A-Za-z]\\.?[0-9]+\\.([0-9]+)")[2]
  day   <- stringr::str_match(x, "^[A-Za-z]\\.?[0-9]+\\.[0-9]+\\.([0-9]+)")[2]
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
    jpyear <- stringr::str_match(x, "[A-Z]")
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

#' @export
jpdate2julian <- function(x) {
  out <- purrr::map(x, jpdate2juliani)
  out <- as.vector(unlist(out))
  out
}
