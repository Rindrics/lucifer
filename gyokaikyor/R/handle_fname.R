give_class <- function(fname, tbl.fname, tbl.fmt) {
  out   <- fname
  prefec <- tbl.fname[[fname]]
  format <- tbl.fmt[[prefec]]
  class(out) <- format
  out
}
