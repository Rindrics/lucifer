#' Check input directory name
#'
#' @param datdir    <- "../data/"
#' @param year_ad   <- 2018 # Not fiscal
#' @param month     <- 10
#' @param pref_name <- "kagoshima"
#' @return String of input directory
#' @examples
#' check_dirname("../data/", 2018, 10, "kagoshima")
#'

make_param <- function(datdir, year_ad, month, pref_name) {
  params <- list()
  params$datdir    <- datdir
  params$year_ad   <- year_ad
  params$month     <- month
  params$pref_name <- pref_name
  params
}

check_dirname <- function(params) {
  params$datdir    <- readr::parse_character(params$datdir)
  params$pref_name <- readr::parse_character(params$pref_name)
  indir     <- paste0(params$datdir, params$year_ad, month.abb[params$month], "/", params$pref_name)
  if (file.exists(indir) == TRUE) {
    ## msg <- paste("OK, the input directory", indir, "exists.\n")
    params$indir <- indir
    return(params$indir)
    } else {
    msg <- "No such dir. \n Please check dirname."
    cat(msg)
  }
}

check_datfiles <- function(indir) {
  list.files(indir)
}

format_data <- function(datdir, year_ad, month, pref_name){
  datdir    <- datdir
  year_ad   <- year_ad
  month     <- month
  perf_name <- pref_name

  param <- make_param(datdir, year_ad, month, pref_name)
  indir <- check_dirname(param)
  check_datfiles(indir)
}




format_data("../data/", 2018, 10, "nagasaki")
(params <- make_param("../data/", 2018, 10, "nagasaki"))
check_dirname(params)

# check_dirname("../data/", 2018, 10, "kagoshima")

# getFiscal   <- function(indir){
#   dirsplit        <- unlist(strsplit(indir, "/"))
#   fiscalyr_char   <- substr(dirsplit[length(dirsplit)], 1, 4)
#   if (gregexpr("[[:digit:]]{4}", fiscalyr_char) < 0){
#     stop(message = "Give me the correct filepath to the year directory.\nThe name format needed is 'path-to-directory/YYYY（年号YY）年度'.")
#   }
#   fiscalyr      <- as.numeric(fiscalyr_char)
#   return(fiscalyr)
# }
