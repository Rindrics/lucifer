#' @export
fmtbl <- function(fname, spcs, nest = FALSE) {
  UseMethod("fmtbl")
}

fmtbl.nagasaki  <- function(fname) {}
fmtbl.kumamoto  <- function(fname, nest = TRUE) {
  out       <- list()
  suppressMessages(
    alldata   <- readxl::read_excel(fname,
                                    sheet = "カタクチ", col_names = FALSE)
  )
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
  unlist(bl)

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
