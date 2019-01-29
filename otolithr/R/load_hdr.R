load_hdr <- function(fname){
  read.csv(fname, fileEncoding = "CP932",
           header = FALSE, stringsAsFactors = FALSE)
}

locate_1stinc <- function(x) {
  which(x == "日輪幅") + 1 # Data of 1stinc is located just after "日輪幅".
}

get_incdata <- function(x) {
  str <- x
  out <- as.numeric(str[locate_1stinc(x):length(str)])
  out
}

load_otolith <- function(fpath) {
  out        <- list()
  data       <- load_hdr(fpath)
  out$fpath  <- fpath
  out$fname  <- fullpath2fname(fpath)
  out$inc    <- get_incdata(data$V1)
  out$radius <- cumsum(out$inc)
  out$ninc   <- length(out$inc)
  out
}
