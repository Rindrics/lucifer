# ts() でもっと簡単に実装できる？
get_range <- function(year.end, year.start) {
  year.start <- year.start
  years      <- year.start:year.end
  n.prefec   <- 6
  maxrow     <- (length(years) + 2) * n.prefec + n.prefec - 1 + 2
  paste0("A1:N", maxrow)
}

load_iwashi <- function(fname, year.end, year.start = 1992, sheet) {
  range   <- get_range(year.end, year.start)
  dat_org <- read_xlsx(fname,
                       sheet = sheet,
                       skip = 2,
                       col_names = FALSE,
                       range = range)
  dat_org
}

conv_df <- function(df) {
  out <- df %>%
    transmute(year = gsub("年", "", X__1) %>% parse_integer(.),
              jan_ton = parse_double(X__2),
              feb_ton = parse_double(X__3),
              mar_ton = parse_double(X__4),
              apr_ton = parse_double(X__5),
              may_ton = parse_double(X__6),
              jun_ton = parse_double(X__7),
              jul_ton = parse_double(X__8),
              aug_ton = parse_double(X__9),
              sep_ton = parse_double(X__10),
              oct_ton = parse_double(X__11),
              nov_ton = parse_double(X__12),
              dec_ton = parse_double(X__13))
  out <- out %>%
    mutate(sum = rowSums(out[, -1], na.rm = TRUE))
  out
}

get_prefec_rows <- function(df) {
  first_col_org <- df$X__1
  row_prefec_org <- (gregexpr("県", first_col_org) > 0) & (gregexpr("西海", first_col_org) < 0)
  row_prefec     <- which((is.na(row_prefec_org) == FALSE) & (row_prefec_org == TRUE) == TRUE)
}

make_list <- function(fname, year.end, year.start = 1992, sheet){
  df         <- load_iwashi(fname, year.end, year.start, sheet)
  years      <- year.start:year.end
  row_prefec <- get_prefec_rows(df)
  out        <- list(prefec = vector("character", length = length(row_prefec)),
                     df = list(matrix(NA,
                                 nrow = length(years),
                                 ncol = 13)))
  for (i in seq_along(row_prefec)) {
    row              <- row_prefec[i]
    prefec_name      <- gsub("県.+", "", df[row, 1])
    row_start        <- row + 2
    row_end          <- row_start + length(years) - 1
    data_pf          <- conv_df(df[row_start:row_end, ])
    out$prefec[i]    <- prefec_name
    out$df[[i]]      <- data_pf
    names(out$df)[i] <- prefec_name
  }
  out
}

summarize_seikai <- function(list, is.gyokaikyo = TRUE) {
  prefecs  <- list$prefec
  df_dummy <- as.data.frame(list$df[1])
  years    <- unlist(unname(df_dummy[1]))
  out      <- matrix(0, nrow = nrow(df_dummy), ncol = ncol(df_dummy))
 for (i in seq_along(prefecs)) {
    df_prefec <- list$df[1]
    if (is.gyokaikyo == TRUE) {
      df <- as.data.frame(unname(list$df[i]))[, -1]
      df[is.na(df)] <- 0
      out <- out + df
    }
  }
  out$year <- years
  out %>%
    select(year, jan_ton:sum)
}
