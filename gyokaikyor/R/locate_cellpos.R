quot_ring <- function(mod, ideal) {
  if (mod == 0) {
    a <- ideal
  } else {
    a <- mod
  }
  a
}

quot2col <- function(quotient, mod) {
  if (mod == 0) {
    col <- quotient
  } else {
    col <- quotient + 1
  }
  col
}

make_RC <- function(row, col) {
  rc <- paste0("R", row, "C", col)
  rc
}

locate_patterns <- function(df, regex) {
  nrows <- dim(df)[1]
  ncols <- dim(df)[2]
  match <- apply(df, 2, gregexpr, pattern = regex) %>%
    unlist()
  pos <- which(match == TRUE)
  quo <- purrr::map(pos, `%/%`, nrows)
  mod <- purrr::map(pos, `%%`, nrows)
  col <- purrr::map2(quo, mod, quot2col)
  row <- purrr::map2(mod, nrows, quot_ring)
  rc  <- purrr::map2(row, col, make_RC)
  pos <- cellranger::R1C1_to_A1(paste0("R", row, "C", col))
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
