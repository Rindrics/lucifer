library(Nippon)
library(tidyverse)
get_tw_sst <- function(fname, year.end) {
  year_start <- 1953
  year_end   <- year.end
  nrows      <- (year_end - year_start + 1) * 12
  range2read <- paste0("A1:F", nrows+1)
  xldata <- read_xlsx(fname,
                      sheet = "三旬平均実測値", range = range2read) %>%
    mutate(tw_degc = 対馬暖流Ｄ,
           year = 0,
           month = 0)

  for(i in 1:nrows){
    xldata[i, "year"]  <- as.numeric(zen2han(gsub("年", "", xldata[i, "X__1"])))
    xldata[i, "month"] <- as.numeric(zen2han(gsub("月", "", xldata[i, "X__2"])))
  }

  sst_tw <- xldata %>%
    dplyr::select(year, month, tw_degc) %>%
    mutate(year = as.integer(year),
           month = as.integer(month))
  sst_tw
}
