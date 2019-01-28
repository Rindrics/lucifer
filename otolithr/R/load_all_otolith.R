load_otolith <- function(dir) {
  type            <- detect_type(dir)
  fname_with_type <- fullpath2fname(dir) %>%
    set_type(type)
  info            <- get_info(fname_with_type)
  out             <- load_otolith(dir)
  out$spcs        <- info$spcs
  # out$age
}
