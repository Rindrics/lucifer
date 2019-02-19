# ----------------------------------------------------------------------------.
# This file is tangled from tinyplyr.org.                                     |
# (https://github.com/smxshxishxad/tinyplyr/blob/master/tinyplyrjmdatar.org)  |
# Do not edit by hand.                                                        |
# ---------------------------------------------------------------------------'

num2date <- function(x) {
  jday_march01 <- 60
  x            <- as.numeric(x)
  if (x == jday_march01) {
    stop("This date is not correct in Excel.")
  }
  if (x > jday_march01) {
    x <- x - 1 # because of leap year
  }
  x <- x - 1 # because of origin
  date <- as.character(as.Date(x, origin = "1900-01-01"))
}
