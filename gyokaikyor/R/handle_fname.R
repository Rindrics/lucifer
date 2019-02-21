give_classi   <- function(fname, prefec) {
  out        <- fname
  class(out) <- prefec
  out
}

#' @export
give_class <- function(fname, tbl.fname) {
  prefec     <- hash::values(tbl.fname, keys = fname)
  out <- purrr::map2(fname, prefec, give_classi)
  out
}
