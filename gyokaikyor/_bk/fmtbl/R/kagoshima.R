source("./_packages.R")
source("./_constants.R")
library(lubridate)
prefec_name        <- "kagoshima"
todoufuken         <- PREFEC_TABLE[[prefec_name]]
gyokaikyo_mar      <- paste0(GYOKAIKYODIR, "/各県資料")
gyokaikyo_oct      <- gsub("3月", "10月", gyokaikyo_mar)
prefecdir          <- list.files(gyokaikyo_oct, pattern = todoufuken, full.names = TRUE)
(infiles            <- list.files(prefecdir, pattern="^[^~]*体長組成.+", full.names = TRUE))
df    <- NULL
for (infile in infiles) {
  daterow        <- 2
  sheets         <- excel_sheets(infile)
  data.org       <- read_xlsx(infile, sheet = which(gregexpr('ｶﾀｸﾁ', sheets)>0))
  dates.org      <- unlist(as.character(as.Date(as.numeric(data.org[daterow,]) - 2, origin="1900-01-01")))
  cols2read      <- which(!is.na(dates.org))
  data           <- data.org[,cols2read]
  data           <- as.tibble(t(data))
  seq_class      <- paste0(formatC(seq(40, 235, 5), width=3, flag=0), "-", formatC(seq(45, 240, 5), width=3, flag=0))
  colnames(data) <- c("rownum", "date", "port", "gyojo", "gear", "meigara", "koumoku", seq_class, "sum")
  data2 <- data %>%
    select(-rownum) %>%
    mutate(date = parse_number(date),
           date = as.Date(date - 2, origin="1900-01-01"),
           year = year(date),
           month = month(date))
  df <- rbind(df, data2)
}
yeardata <- filter(df, year==YEAR)
datcols  <- 7:ncol(yeardata)
for (j in datcols) {
  yeardata[,j] <- yeardata[,j] %>%
    unlist() %>%
    parse_number()
}

out_all <- matrix(NA, nrow=length(seq_class), ncol=12)
for(m in MONTHS){
    mdata   <- filter(yeardata, month==m)
    if (nrow(mdata) > 0) {
      freqs   <- mdata[, 7:ncol(mdata)]
      ifelse(nrow(freqs)==1,
             monsum <- freqs,
             monsum <- colSums(freqs, na.rm = TRUE))
      out_all[,m] <- unlist(monsum[1:length(seq_class)])
    }
}
out_all
col_name        <- paste0(month.abb, YEAR)
rownames(out_all)   <- seq_class
colnames(out_all)   <- col_name
dummy           <- matrix(0, nrow=6, ncol=12)
rownames(dummy) <- CLASSNAME4Excel[1:6]
colnames(dummy) <- col_name

out_all2        <- rbind(dummy, out_all)
out_all3        <- out_all2[rownames(out_all2) %in% CLASSNAME4Excel,]
out_all3        <- as.data.frame(out_all3)
out_all3[is.na(out_all3)] <- 0
write.csv(out_all3, "../output/体長組成_鹿児島.csv")

out_maki <- matrix(NA, nrow=length(seq_class), ncol=12)
for(m in MONTHS){
    mdata_maki<- filter(yeardata, month==m & gregexpr("まき", gear) > 0)
    if (nrow(mdata_maki) > 0) {
      freqs   <- mdata_maki[, 7:ncol(mdata_maki)]
      ifelse(nrow(freqs)==1,
             monsum <- freqs,
             monsum <- colSums(freqs, na.rm = TRUE))
      out_maki[,m] <- unlist(monsum[1:length(seq_class)])
    }
}
out_maki
col_name        <- paste0(month.abb, YEAR)
rownames(out_maki)   <- seq_class
colnames(out_maki)   <- col_name
dummy           <- matrix(0, nrow=6, ncol=12)
rownames(dummy) <- CLASSNAME4Excel[1:6]
colnames(dummy) <- col_name

out_maki2        <- rbind(dummy, out_maki)
out_maki3        <- out_maki2[rownames(out_maki2) %in% CLASSNAME4Excel,]
out_maki3        <- as.data.frame(out_maki3)
out_maki3[is.na(out_maki3)] <- 0
write.csv(out_maki3, "../output/体長組成_鹿児島_まき網のみ.csv")
