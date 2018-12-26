make_params <- function(indir, type){
  if (type %in% c("sengyo", "cruise") == TRUE) {
    param <- list(type = type)
  } else {
    stop("Tell me the correct type of data. Is it 'sengyo', or 'cruise?'")
  }
  param$indir <- indir
  param
}
make_datlist <- function(param){
  if (param$type == "sengyo") {
    datlist <- list.files(param$indir, pattern = "鮮魚")
  }
  datlist
}

parse_year <- function(datlist){
  yearlist <- datlist %>%
    str_sub(5, 6) %>%
    paste0(20, .)
  yearlist
}

get_filelist <- function(param) {
  if (is.na(param$spcs)) {
    stop("Give me Japanese species name")
  } else {
    regexp   <- paste0(param$spcs, ".+")
  }
  filelist <- list.files(param$indir, pattern = regexp, recursive = TRUE, full.names = TRUE)
  filelist
}

get_sheet2read <- function(infile) {
  all_sheets <- readxl::excel_sheets(infile)
  sheets2read <- as.vector(na.omit(str_match(all_sheets, "^(?!.*0000)(?!体長)(?!Sheet).+")))
  sheets2read
}
# get_date <- function(year, sheetname) {
#   date_char <- if_else(str_length(sheetname) >= 9,
#                        paste0(20, str_sub(sheetname, 1, 6)),
#                        paste0(year, str_sub(sheetname, 1, 4)))
#   date <- ymd(date_char)
#   date
# }

# format_sengyo <- function(indir, spcs_name) {
#   out      <- NULL
#   yearlist <- get_year(indir)
#   filelist <- get_filelist(indir, spcs_name)
#   for (i in seq_along(filelist)) {
#     infile      <- filelist[i]
#     year        <- yearlist[i]
#     sheets2read <- get_sheet2read(infile)
#     print(year)
#     for (j in seq_along(sheets2read)) {
#       sheetname <- sheets2read[j]
#       date <- get_date(year, sheetname)
#       data <- read_xls(infile, sheet = sheetname) %>%
#         mutate(date = date,
#                original.fname = infile,
#                original.sheetname = sheetname)
#       print(sheetname)
#       out  <- bind_rows(out, data)
#     }
#   }
#   out
                                      # }
