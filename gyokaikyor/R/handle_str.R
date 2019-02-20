get_col2load   <- function(target, regex, offset) {
  match <- stringr::str_detect(target, regex)
  out <- which(match == TRUE) + offset
  out
}
