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


}
