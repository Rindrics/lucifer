library(tidyverse)
taichou_all <- read.csv(./data/frescodata.csv)
pdata <- taichou_all %>% dplyr::filter(県コード == 38) %>%
  mutate(date = parse_date(as.character(漁獲年月日), "%Y%m%d")) %>%
  separate(date, c("year", "month", "day"), sep = "-") %>%
  mutate(month = month.abb[as.numeric(month)], sep = "-") %>%
  tidyr::unite("yearmonth", c("year", "month")) %>%
  select(開始の階級値, yearmonth, 度数) %>%
  arrange(開始の階級値) %>%
  group_by(yearmonth) %>% data.frame() %>% print()
