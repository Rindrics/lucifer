get_path <- function(dir.spcs) {
  regex <- ".+hdr$"
  fullpaths <- list.files(dir.spcs, pattern = regex,
                          full.names = TRUE, recursive = TRUE)
  fullpaths
}

get_dir2load <- function(paths) {
  regex         <- ".+\\/data\\/.+\\.hdr$"
  (match_length <- purrr::map(regex, gregexpr, paths) %>% unlist())
  match_pos     <- which(match_length > 0)
  dir2load      <- paths[match_pos]
  dir2load
}
