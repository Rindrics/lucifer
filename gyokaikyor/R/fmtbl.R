#' @export
fmtbl <- function(fname, spcs, nest = FALSE) {
  UseMethod("fmtbl")
}

load_alldata <- function(fname, shtname) {
  suppressMessages(
    alldata   <- readxl::read_excel(fname,
                                    sheet = shtname, col_names = FALSE)
  )
}

make_blclass <- function(left, right) {
  left %<>% unlist() %>%
    as.vector() %>%
    as.numeric() %>%
    formatC(width = 3, flag = 0)
  right %<>% unlist() %>%
    as.vector() %>%
    as.numeric() %>%
    formatC(width = 3, flag = 0)
  out <- paste(left, right, sep = "-")
  out
}

get_histdata <- function(df, col, rows, name) {
  out <- df[rows, col] %>%
    unlist() %>%
    as.numeric() %>%
    as.vector()
  names(out) <- name
  out
}

jpmonth2num <- function(x) {
  out <- x %>%
    as.vector() %>%
    gsub("月", "", .) %>%
    as.numeric()
  out
}

fmtbl.nagasaki  <- function(fname, spcs, nest = TRUE) {
  shtname   <- make_shtname(prefecture = "nagasaki", spcs = spcs)
  alldata   <- load_alldata(fname, shtname)
  colpos    <- get_col2load(target = alldata[4,], regex = ".月", offset = 0)
  month     <- jpmonth2num(alldata[4, colpos])
  classname <- make_blclass(alldata[5:86, 2], alldata[5:86, 3])
  histdata  <- purrr::map(colpos, get_histdata, df = alldata, rows = 5:86, name = classname)

  out       <- list()

  parse_ym <- function(fname, months) {
    ym_start_match <- stringr::str_match(fname, "/([0-9]{4})\\.((?:0|1)[1-9])")
    year_start     <- ym_start_match[2] %>% as.numeric()
    month_start    <- ym_start_match[3] %>% as.numeric()
    ym_end_match   <-
      stringr::str_match(fname,
                         "/[0-9]{4}\\.(?:0|1)[1-9]-([0-9]{4})\\.((?:0|1)[1-9])")
    year_end       <- ym_end_match[2] %>% as.numeric()
    month_end      <- ym_end_match[3] %>% as.numeric()
    out <- list()
    out$year_start  <- year_start
    out$month_start <- month_start
    out$year_end    <- year_end
    out$month_end   <- month_end
    out
  }
  parsedym <- parse_ym(fname, month)

  check_month <- function(months, month_start, month_end) {
    if (!(month_start == months[1]) | (!month_end == rev(months)[1])) {
      stop ("Check month data")
    }
  }
  check_month(month, parsedym$month_start, parsedym$month_end)

  give_yr2month <- function(mvec, year.start) {
    out           <- list()
    is_yr_changed <- FALSE
    for (i in seq_along(mvec)) {
       m            <- mvec[i]
       out$month[i] <- m

       if (i >= 2) {
         if (m < out$month[i - 1]) {
         is_yr_changed <- TRUE
         }
       }

       if (is_yr_changed) {
         out$year[i] <- year.start + 1
       } else {
         out$year[i] <- year.start
       }
    }
    out
  }

  year_start <- parsedym$year_start
  out$year   <- give_yr2month(month, year_start)$year
  out$month  <- give_yr2month(month, year_start)$month
  out$hist   <- histdata
  out        <- tibble::as_tibble(out)
  if (nest == FALSE) {
    out <- tidyr::unnest(out)
  }
  out
}

fmtbl.kumamoto  <- function(fname, spcs, nest = TRUE) {
  shtname   <- make_shtname(prefecture = "kumamoto", spcs = spcs)
  alldata   <- load_alldata(fname, shtname)
  cpos_date <- get_col2load(alldata[1,], regex = "[0-9]+", offset = 0)
  cpos_bl   <- cpos_date # Column position of "被鱗体長" is
                         #   same as that of "%Y.%m.%d" column
  date      <- alldata[1, cpos_date] %>%
    tinyplyr::jpdate2julian() %>%
    tinyplyr::num2date()
  method    <- alldata[1, cpos_date + 4] %>%
    unlist() %>%
    as.vector()
  bl        <- purrr::map(cpos_date, get_measdata, df = alldata)

  out        <- list()
  out$date   <- date
  out$method <- method
  out$year   <- lubridate::year(out$date)
  out$month  <- lubridate::month(out$date)
  out$bl     <- bl

  out <- tibble::as_tibble(out)
  if (nest == FALSE) {
    out <- tidyr::unnest(out)
  }
  out
}
