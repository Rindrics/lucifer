# Copied from /Users/ahayashi/Documents/GitHub/stockAssess/BLhist/Yamaguchi_Shimane.R on 2018-10-09
source("./_packages.R")
source("./_constants.R")

prefec_list                 <- c("山口", "島根", "佐賀", "長崎")
empty_histdata_mm           <- as.tibble(cbind(CLASSNAME4Excel, 0)) %>%
    transmute(開始の階級値 = as.numeric(str_sub(CLASSNAME4Excel, 1, 3)),
              度数 = as.integer(V1))

indir                       <- "../../data"
taichou_files               <- list.files(indir, pattern = "taichou.+csv", full.names = TRUE)
taichou_data_org                <- NULL
for(f in taichou_files){
  data <- read_csv(f, locale = locale(encoding = "cp932"))
  taichou_data_org <- rbind(taichou_data_org, data)
}
seimitsu_files              <- list.files(indir, pattern = "seimitsu.+csv", full.names = TRUE)
seimitsu_data_org               <- NULL
for(f in seimitsu_files){
  data <- read_csv(f, locale = locale(encoding = "cp932"))
  seimitsu_data_org <- rbind(seimitsu_data_org, data)
}

# summarize "seimitsu" data from fresco
seimitsu_data <- seimitsu_data_org %>%
  mutate(date = parse_date(漁獲年月日, "%Y%m%d")) %>%
  separate(date, c("year", "month", "day"), sep = "-") %>%
  mutate(year = as.integer(year),
         month = as.integer(month),
         day = as.integer(day)) %>%
  dplyr::filter(year == YEAR) %>%
  dplyr::filter(県コード != PREFEC_CODE_TBL[["島根"]] & year != year-1 & month >= 4) %>% # 1〜3月は鳥取県が島根県として入力していた
  select(year, month, day, 魚種コード, 県コード, 被鱗体長, 体重)

for(p in prefec_list){
  if (p == "長崎") {
    org_name     <- "西海区"
  } else {
    org_name     <- p
  }
  pcode        <- PREFEC_CODE_TBL[[p]]
  out_seimitsu <- matrix(0, nrow = length(CLASSNAME4Excel), ncol = length(MONTHS))
  for(m in MONTHS){
    mdata <- seimitsu_data %>%
      dplyr::filter(県コード == pcode & month == m)
      # print()
  histdata <- hist(mdata$被鱗体長, breaks = BREAKS4Excel, plot = FALSE, right = FALSE)
  out_seimitsu[, m] <- histdata$counts
  }
  out_seimitsu %>% data.frame()
  colnames(out_seimitsu) <- paste0(month.abb, YEAR)
  rownames(out_seimitsu) <- CLASSNAME4Excel
  out_seimitsu
  write.csv(out_seimitsu, paste0("../output/体長組成_", org_name, "_精密.csv"))
  write.csv(out_seimitsu, paste0("../output/体長組成_", org_name, ".csv"))
}

# summarize "taichou" data from fresco
taichou_data <- taichou_data_org %>%
  mutate(date = parse_date(漁獲年月日, "%Y%m%d")) %>%
  separate(date, c("year", "month", "day"), sep = "-") %>%
  mutate(year = as.integer(year),
         month = as.integer(month),
         day = as.integer(day)) %>%
  dplyr::filter(year == YEAR) %>%
  select(year, month, day, 魚種コード, 県コード, 開始の階級値, 度数)
  # print()

for(p in prefec_list){
  if (p == "長崎") {
    org_name     <- "西海区"
  } else {
    org_name     <- p
  }
  pcode        <- PREFEC_CODE_TBL[[p]]
  out_taichou  <- matrix(0, nrow = length(CLASSNAME4Excel), ncol = length(MONTHS))
  #if (p == "佐賀"){browser()}
  for (m in MONTHS) {
    mdata <- taichou_data %>%
      dplyr::filter(県コード == pcode & month == m)
    mdata$開始の階級値
    if (min(mdata$開始の階級値) < min(BREAKS4Excel)) {
      mdata$開始の階級値 <- mdata$開始の階級値 * 10
    }
    histdata <- full_join(mdata, empty_histdata_mm, by = "開始の階級値") %>%
        replace(is.na(.), 0) %>%
        mutate(count = 度数.x + 度数.y) %>%
        arrange(開始の階級値) %>%
        group_by(開始の階級値) %>%
        summarize(sum(count))
    out_taichou[, m] <- unlist(histdata$`sum(count)`)[1:length(CLASSNAME4Excel)]
  }
  out_taichou %>% data.frame()
  colnames(out_taichou) <- paste0(month.abb, YEAR)
  rownames(out_taichou) <- CLASSNAME4Excel
  out_taichou
  write.csv(out_taichou, paste0("./output/体長組成_", org_name, "_体長.csv"))
  if (p == "山口" | p == "佐賀" | p == "島根") {
    write.csv(out_taichou, paste0("./output/体長組成_", org_name, ".csv"))
  }
}
