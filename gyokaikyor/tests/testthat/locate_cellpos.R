locate_patterns <- function(df, regex) {
  match <- apply(df, 2, gregexpr, pattern = regex) %>%
    unlist()
  pos <- which(match == TRUE) %>%
    names()
  pos
}

get_topleft <- function(df, regex) {
  pos <- locate_patterns(df, regex)
  pos[1]
}

get_bottomright <- function(df, regex) {
  pos <- locate_patterns(df, regex)
  rev(pos)[1]
}
