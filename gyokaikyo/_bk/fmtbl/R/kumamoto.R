# data[duplicated(paste(data$Date, data$No, sep = ",")), ]
source("./_packages.R")
source("./_constants.R")
library(lubridate)
prefecture          <- "kumamoto"
prefec_name         <- PREFEC_TABLE[[prefecture]]
species             <- c("Sardine", "Anchovy", "Round herring", "Mackerels")
spname_translate    <- hash(species, c("マイワシ", "カタクチイワシ", "ウルメイワシ", "サハ゛類")) #do not use character "サバ"
header_width        <- 7
rowpos_of_1stdata   <- 8
offset_tl           <- -1
colshift_bw         <- 3
colshift_sex        <- 4
colshift_gw         <- 5
colshift_date       <- 2
colpos_of_hist      <- 3
n_of_histclass      <- 45
dattype             <- c("体長", "精密")
out                 <- NULL
need_check          <- NULL
gyokaikyo_oct       <- gsub("3月", "10月", GYOKAIKYODIR)
bldatdir            <- paste(gyokaikyo_oct, "/各県資料/",PREFEC_TABLE[[prefecture]],"県", sep="")
infiles              <- list.files(bldatdir, recursive = TRUE, full.names = TRUE, pattern="^[^~$].+体長.+")
char2num <- function(df) {
  vec <- df %>% unlist() %>% parse_number()
  vec
}
char2date <- function(df) {
  num <- df %>% unlist() %>% parse_number()
  date <- as.Date(num - 2, origin="1900-01-01")
}
out <- NULL
for(f in infiles){
  if (gregexpr("xlsx", f) > 0) {
    data        <- read_xlsx(f, sheet="カタクチ", col_names = FALSE)
  } else {
    data        <- read_xls(f, sheet="カタクチ", col_names = FALSE)
  }
  datcols     <- NULL
  date.org  <- data[1,]
  cols2read <- which((!is.na(date.org) & (gregexpr(".+[0-9].+", date.org) > 0)) == TRUE)
  nacnt     <- 0
  df        <- matrix(NA, nrow=100*length(cols2read), ncol=7)
  cnt       <- 0
  for(j in cols2read){
    pos2replace <- 1:100 + 100 * cnt
    date <- char2date(date.org[j])
    if (is.na(date) == TRUE) {browser()}
    total.length_mm <- data[rowpos_of_1stdata:nrow(data), j+offset_tl] %>% char2num
    bl_mm           <- data[rowpos_of_1stdata:nrow(data), j] %>% char2num()
    sample_no       <- 1:length(bl_mm)
    bw_g            <- data[rowpos_of_1stdata:nrow(data), j + 1] %>% char2num()
    sex             <- data[rowpos_of_1stdata:nrow(data), j + 2] %>% char2num() # male: 1; female:2
    gw              <- data[rowpos_of_1stdata:nrow(data), j + 3] %>% char2num()
    year      <- year(date)
    month     <- month(date)
    day       <- day(date)
    fisheries_type  <- as.character(data[1, j+header_width-3])
    ifelse(gregexpr("[巻ま]き", fisheries_type)>0,
           fisheries_type <- "Makiami",
           ifelse(gregexpr("棒受け?", fisheries_type)>0,
           fisheries_type <- "Bouukeami",
           ""))
    df[pos2replace, 1] <- sample_no
    df[pos2replace, 2] <- as.character(date)
    df[pos2replace, 3] <- fisheries_type
    df[pos2replace, 4] <- bl_mm
    df[pos2replace, 5] <- bw_g
    df[pos2replace, 6] <- sex
    df[pos2replace, 7] <- gw
    cnt <- cnt + 1
  }
  out <- rbind(out, df)
}
out <- as.tibble(out)
colnames(out) <- c("sample.no", "date","fisheries.type","bl_mm","bw_g","sex","gw_g")
out <- out[!is.na(out$bl_mm),]

out2 <- out %>%
  mutate(sample.no = parse_number(sample.no),
         date = parse_date(date),
         year = year(date),
         month = month(date),
         day = day(date),
         bl_mm = parse_number(bl_mm),
         bw_g = parse_number(bw_g),
         sex = parse_integer(sex),
         gw_g = parse_number(gw_g))
tail(out2)

out2 %>%
  ggplot(aes(bl_mm, bw_g)) +
  geom_point()

formatted_data <- subset(out2, year==YEAR)
out_all  <- matrix(NA, nrow = length(CLASSNAME4Excel), ncol = 12)
out_maki <- matrix(NA, nrow = length(CLASSNAME4Excel), ncol = 12)
for(m in MONTHS){
  # m=6
  mdata   <- filter(formatted_data, month==m)
  if(nrow(mdata)>0){
      hist <- hist(mdata$bl_mm, breaks=BREAKS4Excel, right=F)$counts
  }else{
      hist <- rep(0, length(CLASSNAME4Excel))
  }
  out_all[,m]        <- hist

  mdata_maki <- filter(formatted_data, month==m & fisheries.type == "Makiami")
  if(nrow(mdata_maki) > 0){
      hist_maki <- hist(mdata_maki$bl_mm, breaks=BREAKS4Excel, right=FALSE)$counts
  }else{
      hist_maki<- rep(0, length(CLASSNAME4Excel))
  }
  out_maki[,m]        <- hist_maki
}

outfname    <- paste0("../", OUTDIR, "/体長組成_熊本.csv")
rownames(out_all) <- CLASSNAME4Excel
colnames(out_all) <- paste0(month.abb, YEAR)
out_all           <- out_all[rownames(out_all) %in% CLASSNAME4Excel, ]
write.csv(out_all, outfname)
outfname_maki      <- paste0("../", OUTDIR, "/体長組成_熊本_まき網のみ.csv")
rownames(out_maki) <- CLASSNAME4Excel
colnames(out_maki) <- paste0(month.abb, YEAR)
out_maki           <- out_maki[rownames(out_maki) %in% CLASSNAME4Excel, ]
write.csv(out_maki, outfname_maki)

print(out_all)
