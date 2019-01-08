# This script is tangled from reformr.org.
# Do not edit by hand!!!
make_params <- function(indir, type, spcs){
  spcs_list <- c("カタクチイワシ", "マイワシ", "マアジ")
  param <- list()
  if (type %in% c("sengyo", "cruise") == TRUE) {
    param$type = type
  } else {
    stop("Tell me the correct type of data. Is it 'sengyo', or 'cruise?'")
  }
  if (spcs %in% spcs_list) {
    param$spcs <- spcs
  } else {
    stop("Tell me the correct Japanese species name.")
  }
  param$indir <- indir
  param
}
make_datlist <- function(indir, type){
  if (type == "sengyo") {
    datlist <- list.files(indir, pattern = "鮮魚")
  }
  datlist
}

parse_year <- function(datlist){
  yearlist <- datlist %>%
    str_sub(5, 6) %>%
    paste0(20, .)
  yearlist
}

get_filelist <- function(indir, spcs) {
  if (is.na(spcs)) {
    stop("Give me Japanese species name")
  } else {
    regexp   <- paste0(spcs, ".+")
  }
  filelist <- list.files(indir, pattern = regexp, recursive = TRUE, full.names = TRUE)
  filelist
}

get_sheet2read <- function(infile) {
  all_sheets <- readxl::excel_sheets(infile)
  sheets2read <- as.vector(na.omit(stringr::str_match(all_sheets, "^(?!.*0000)(?!体長)(?!Sheet).+")))
  sheets2read
}
get_date <- function(year, sheetname) {
  date_char <- dplyr::if_else(str_length(sheetname) >= 9,
                       paste0(20, str_sub(sheetname, 1, 6)),
                       paste0(year, str_sub(sheetname, 1, 4)))
  date      <- lubridate::ymd(date_char)
  date
}

format <- function(infile, sheet) {
  data_org <- readxl::read_xls(infile, sheet = sheet)
  col_names <- colnames(data_org)
  if (col_names[1] == "番号") data_org <- rename(data_org, No = "番号")
  if (any(regexpr("耳石", col_names) >0 ) == FALSE) data_org <- mutate(data_org, 耳石 = 0) 
  data <- data_org %>%
    transmute(sample.no = parse_integer(No),
              bl_mm = parse_double(BL),
              bw_g = parse_double(BW),
              sex = parse_integer(Sex),
              gw_g = parse_double(GW),
              gsi = gw_g / bw_g * 100,
              otolith.taken = 耳石 %>%
                str_replace("y", "1") %>%
                parse_integer()) %>%
    mutate(original.fname = infile,
           original.sheetname = sheet)
  data
}
