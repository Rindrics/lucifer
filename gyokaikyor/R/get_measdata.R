get_measdata <- function(df, col) {
  rows <- 8:107
  out  <- df[rows, col] %>%
    unlist() %>%
    as.numeric() %>%
    na.omit() %>%
    as.vector()
  out
}
