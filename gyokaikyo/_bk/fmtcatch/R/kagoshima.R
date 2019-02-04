format_kagoshima <- function(indir){
  data.kagoshima  <- structure(list(), class="PrefecCatch")
  indir.kagoshima   <- paste(INDIR, "18鹿児島", sep = "/")

  JP.FISHNAME     <- c("マイワ","カタク","ウルメ","サバ類","マアジ")
  GYOSHU.LIST     <- c("maiwashi", "katakuchi", "urume", "sabarui", "maaji")
  SPNAME_JP       <- hash::hash(JP.FISHNAME, GYOSHU.LIST)
  TARGET.SHEETS     <- c("４港計","阿久根棒受","内之浦棒受" )
  FISHERY.TYPES     <- c("chuukomaki", "bouuke", "bouuke")
  FISHERY.TYPE.TBL  <- hash(TARGET.SHEETS, FISHERY.TYPES)
  FISCAL.YEAR       <- getFiscal(INDIR)
  MONTHS            <- 1:12
  date                <- ""
  day                 <- ""
  out                 <- NULL
  meigara             <- "Need to be implement"
  prefecture          <- "kagoshima"

  COLPOS_MONTH        <- 1
  COLOFFSET_SUM       <- 4
  ROWSKIP_TBL           <- hash::hash(TARGET.SHEETS, c( 1, 0, 0))
  ENDROW_TBL            <- hash::hash(TARGET.SHEETS, c(15,13,13))
  ROWOFFSET_MON_TBL     <- hash::hash(TARGET.SHEETS, c( 2, 1, 1))
  SPNAMEROW_TBL         <- hash::hash(TARGET.SHEETS, c( 1, 1, 1))

 list.files(indir.kagoshima, pattern="^[^~$]4|４港.+")

  CATCHFILELIST       <- list.files(indir.kagoshima, pattern="^[^~$]4?４?港.+")
  f                   <- CATCHFILELIST
  if (is.na(f) == TRUE){
    stop("File missing!")
  }
  # for(f in CATCHFILELIST){
  # }

  filepath         <- paste(indir.kagoshima, f,  sep="/")
  all.sheet.names  <- readxl::excel_sheets(filepath)
  is.target.exist  <- sum(readxl::excel_sheets(filepath) %in% TARGET.SHEETS) == 3
  stopifnot(is.target.exist)


  out         <- NULL
  for(sheet in TARGET.SHEETS){
  fishery.type  <- FISHERY.TYPE.TBL[[sheet]]
  row.skip      <- ROWSKIP_TBL[[sheet]]
  end.row       <- ENDROW_TBL[[sheet]]
    data        <- readxl::read_excel(filepath,
                                      sheet = sheet,
                                      skip = row.skip,
                                      n_max = end.row
                                      )
    rowoffset_mon   <- ROWOFFSET_MON_TBL[[sheet]]
    for(j in 1:ncol(data)){
      pre_gyoshuname    <- as.character(data[SPNAMEROW_TBL[[sheet]], j])
      pre_gyoshuname    <- gsub("　", "", pre_gyoshuname)
      gyoshuname_3char  <- substr(pre_gyoshuname,1,3)
      if(is.null(SPNAME_JP[[gyoshuname_3char]])==F){
    # To get position of month and catchdata correctly by species
        if(gyoshuname_3char %in% c("マアジ", "サバ類")){
          if(gregexpr("計", pre_gyoshuname)>0){
            gyoshu <- SPNAME_JP[[gyoshuname_3char]]
            for(i in 1:length(MONTHS)){
              month <- i + 3*(i<=9) -9*(i>=10)
              catch_kg <- as.numeric(as.character(data[i+rowoffset_mon, j]))
              if(month<=3){
                year <- FISCAL.YEAR+1
              }else if(month>=4){
                year <- FISCAL.YEAR
              }
              # out     <- rbind(out, cbind(date, year, month, day, p, sh, gyoshu, catch_kg))
            }
          }
        }else{
          gyoshu <- SPNAME_JP[[gyoshuname_3char]]
          for(i in 1:length(MONTHS)){
            month <- i + 3*(i<=9) -9*(i>=10)
            catch_kg <- as.numeric(as.character(data[i+rowoffset_mon, j]))
            if(month<=3){
              year <- FISCAL.YEAR+1
            }else if(month>=4){
              year <- FISCAL.YEAR
            }
            out     <- rbind(out, cbind(gyoshu, meigara, prefecture, fishery.type, year, month, day, catch_kg))
          }
        }
      }
    }
  }
  out           <- as.data.frame(out)
print(summary(out))
  # print(paste("Completed ", p, "!", sep=""))
}

# format_kagoshima(INDIR)
