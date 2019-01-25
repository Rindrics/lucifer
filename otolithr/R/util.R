#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


format <- function(x) {
  UseMethod(format)
}

# format.otolith <- function(dir, spcs){

# }

# get_dir <- function(dir) {

# }

rm_extension <- function(x, extension = ".hdr") {
  if (gregexpr("^[a-zA-Z]+", extension) > 0)
    stop("'extension' must begin with '.'")
  regex <- paste0("\\", extension)
  out   <- sub(regex, "", x)
}

split_fname <- function(fname, sep = "_") {
  split <- strsplit(fname, sep) %>%
    unlist() %>%
    as.vector()
  split
}

detect_type <- function(fpath) {
  regex <- "(survey|commercial|reared)"
  type  <- stringr::str_match(fpath, regex)[,2]
  if (is.na(type))
    stop("Dir structure error. Run 'help(detect_type)'.")
  type
}

set_type <- function(fname, type) {
  if (!(type %in% c("survey", "commercial", "reared")))
    stop("'type' must be one of 'survey', 'commercial', or 'reared'.")
  class(fname) <- type
  fname
}

get_info <- function(fname) {
  UseMethod("get_info")
}


get_info.survey <- function(fname) {
  out <- list()
  class(out)   <- "survey"
  out$spcs     <- split_fname(fname)[1]
  out$crs.name <- split_fname(fname)[2]
  out$stn      <- split_fname(fname)[3]
  out$sampleno <- split_fname(fname)[4] %>% rm_extension(".hdr")
  out
}
