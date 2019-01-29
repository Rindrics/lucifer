#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`



rm_extension <- function(x, extension = ".hdr") {
  if (gregexpr("^[a-zA-Z]+", extension) > 0)
    stop("'extension' must begin with '.'")
  regex <- paste0("\\", extension)
  out   <- sub(regex, "", x)
}

split_fname <- function(fname, sep = "_") {
# This function may be unnecessary.
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

xtract_var <- function(fname, var) {
  switch (var,
    "spcsname" = regex <- "(^[A-Z][a-z]+-[a-z]+)_",
    "cruise"   = regex <- "^[A-Z][a-z]+-[a-z]+_([A-Za-z0-9]+)_",
    "stn"      = regex <- "^[A-Z][a-z]+-[a-z]+_[A-Za-z0-9]+_([A-Za-z0-9]+)_",
    "date"     = regex <- "_(2[0-9]{7})_",
    "key1"     =
      regex <- "_2[0-9]{7}_([A-Za-z0-9]+)_(?:[A-Za-z]+_)?[a-zA-Z0-9]+\\.hdr$",
    "key2"     =
      regex <- "_2[0-9]{7}_(?:[A-Za-z]+)_([A-Za-z0-9]+)_[a-zA-Z0-9]+\\.hdr$",
    "sampleno" = regex <- "_([a-zA-Z0-9]+)\\.hdr$",
    "fname"    = regex <- "(^.+$)",
    stop(paste0("Unexpected variable '", eval(bquote(var)), "' was given."))
  )
  out <- stringr::str_match(fname, regex)[,2]
  out
}


get_info <- function(fname) {
  UseMethod("get_info")
}

get_info.survey <- function(fname) {
  out <- list()
  out$type     <- "survey"
  out$fname    <- xtract_var(fname, "fname")
  out$spcs     <- xtract_var(fname, "spcsname")
  out$crs.name <- xtract_var(fname, "cruise")
  out$stn      <- xtract_var(fname, "stn")
  out$sampleno <- xtract_var(fname, "sampleno")
  out
}

get_info.commercial <- function(fname) {
  out <- list()
  out$type     <- "commercial"
  out$fname    <- xtract_var(fname, "fname")
  out$spcs     <- xtract_var(fname, "spcsname")
  out$date     <- xtract_var(fname, "date")
  out$key1     <- xtract_var(fname, "key1")
  out$key2     <- xtract_var(fname, "key2")
  out$sampleno <- xtract_var(fname, "sampleno")
  out
}

get_info.reared <- function(fname) {
  out <- list()
  out$type     <- "reared"
  out$fname    <- xtract_var(fname, "fname")
  out$spcs     <- xtract_var(fname, "spcsname")
  out$date     <- xtract_var(fname, "date")
  out$key1     <- xtract_var(fname, "key1")
  out$key2     <- xtract_var(fname, "key2")
  out$sampleno <- xtract_var(fname, "sampleno")
  out
}
