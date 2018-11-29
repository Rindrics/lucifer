get_year <- function(indir) {
 yearlist <- list.files(indir, pattern = "鮮魚") %>%
   str_sub(5, 6) %>%
   paste0(20, .) %>%
   parse_number()
 yearlist
}

get_filelist <- function(indir, spcs_name) {
  regexp   <- paste0(spcs_name, ".+")
  filelist <- list.files(indir, pattern = regexp, recursive = TRUE, full.names = TRUE)
  filelist
}

get_sheet2read <- function(infile) {
  all_sheets <- excel_sheets(infile)
  sheets2read <- all_sheets[gregexpr("[^Sheet][^体長][^0000(1)].+", all_sheets) > 0]
  sheets2read
}
get_date <- function(year, sheetname) {
  date_char <- if_else(str_length(sheetname) >= 9,
                       paste0(20, str_sub(sheetname, 1, 6)),
                       paste0(year, str_sub(sheetname, 1, 4)))
  date <- ymd(date_char)
  date
}

format_sengyo <- function(indir, spcs_name) {
  out      <- NULL
  yearlist <- get_year(indir)
  filelist <- get_filelist(indir, spcs_name)
  for (i in seq_along(filelist)) {
    infile      <- filelist[i]
    year        <- yearlist[i]
    sheets2read <- get_sheet2read(infile)
    print(year)
    for (j in seq_along(sheets2read)) {
      sheetname <- sheets2read[j]
      date <- get_date(year, sheetname)
      data <- read_xls(infile, sheet = sheetname) %>%
        mutate(date = date,
               original.fname = infile,
               original.sheetname = sheetname)
      print(sheetname)
      out  <- bind_rows(out, data)
    }
  }
  out
}
