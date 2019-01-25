get_path <- function(dir.spcs) {
  regex <- ".+hdr$"
  fullpaths <- list.files(dir.spcs, pattern = regex,
                          full.names = TRUE, recursive = TRUE)
  fullpaths
}

fullpath2fname <- function(full.path) {
  regex <- "/([^/]+\\.hdr)$"
  if (length(full.path) == 1) {
    fname <- stringr::str_match(full.path, regex)[2]
  } else {
    match_res <- purrr::map2(full.path, regex, stringr::str_match) %>% unlist()
    fname     <- match_res[!(1:length(match_res))%%2]
  }
  fname
}

get_dir2load <- function(paths) {
  regex         <- ".+\\/data\\/.+\\.hdr$"
  (match_length <- purrr::map(regex, gregexpr, paths) %>% unlist())
  match_pos     <- which(match_length > 0)
  dir2load      <- paths[match_pos]
  dir2load
}
