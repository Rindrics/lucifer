give_class   <- function(fname, tbl.fname) {
  out        <- fname
  prefec     <- tbl.fname[[fname]]
  class(out) <- prefec
  out
}
