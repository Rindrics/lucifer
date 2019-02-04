print(paste("Processing catchdata in ", p, "...", sep=""))
COLPOS_FISCALYEAR   <- 1
COLPOS_MONTH_START  <- 3
ROWPOS_YEAR_START   <- 3
SPNAME_TBL          <- hash(c("ﾏｲﾜ","ｶﾀｸ","ｳﾙﾒ","ﾏｻﾊ","ﾏｱｼ"), SPCS_LIST)
date                <- ""
day                 <- ""
out                 <- NULL
for(f in fname_prefec){
  yearfile          <- paste(INDIR, FISCAL_YEARS, p, f, sep="/")
  sheets_yearfile   <- excel_sheets(yearfile)
  if(sum(sheets_yearfile==c("集計区分概念図","ﾏｱｼﾞ（唐津港）","ﾏｱｼﾞ（沿岸漁具）","ﾏｱｼﾞ（定置）","ﾏｻﾊﾞ（唐津港）","ﾏｻﾊﾞ（沿岸漁具）","ﾏｻﾊﾞ（定置）","ﾏｲﾜｼ（唐津港）",
                                 "ﾏｲﾜｼ（沿岸漁具）","ﾏｲﾜｼ（定置）","ｳﾙﾒ（沿岸漁具）","ｳﾙﾒ（定置）","ｶﾀｸﾁ（沿岸漁具）","ｶﾀｸﾁ（定置）","ｶﾀｸﾁ（中まき）","Sheet1"))!=16){
    print(paste("Change detected in the", p, "file!!"))
    browser()
  }else{
    sheets_yearfile <- sheets_yearfile[c(-1, -16)]
  }
  for(sh in sheets_yearfile){
    data              <- read.xlsx(yearfile, sheetName = sh)
    for(i in ROWPOS_YEAR_START:nrow(data)){
      if(is.na(as.character(data[i, COLPOS_FISCALYEAR]))==T){
        break
      }else if(as.character(data[i, COLPOS_FISCALYEAR])==""){
        break
      }
    }
    endrow              <- i - 1
    data                <- data[1:endrow,]
    spname_3chr         <- substr(sh, 1, 3)
    sp                  <- SPNAME_TBL[[spname_3chr]]
    for(j in COLPOS_MONTH_START:(COLPOS_MONTH_START+(length(MONTHS)-1))){
      if(j <= 11){
        month     <- j+1
        year      <- as.numeric(as.character(data[ROWPOS_YEAR_START:nrow(data),COLPOS_FISCALYEAR]))
      }else{
        month     <- j-11
        year      <- as.numeric(as.character(data[ROWPOS_YEAR_START:nrow(data),COLPOS_FISCALYEAR]))+1
      }
      catch_kg    <- as.character(data[ROWPOS_YEAR_START:nrow(data),j])
    }
    out       <- rbind(out, cbind(date, year, month, day, p, sh, sp, catch_kg))
  }
}
out           <- as.data.frame(out)
colnames(out) <- c("Date","Year","Mon","Day", "Prefecture", "FromSheetNamed", "Species","Catch_kg")
out_all       <- rbind(out_all, out)
# head(out)
print(paste("Completed ", p, "!", sep=""))
