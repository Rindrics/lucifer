print(paste("Processing catchdata in ", p, "...", sep=""))
COLPOS_DATSTART <- 2
COLPOS_SPNAME   <- 1
YEAR_OFFSET     <- 1
YEAR_START      <- 2005
date            <- ""
day             <- ""
out             <- NULL
for(f in fname_prefec){
  yearfile          <- paste(INDIR, FISCAL_YEARS, p, f, sep="/")
  sheets_yearfile   <- excel_sheets(yearfile)
  if(sum(c("漁獲量・努力量（全県中まき）", "漁獲量・努力量（浜田中まき）") %in% sheets_yearfile)!=2){
    print("Change in Excel sheetname was found in Tottori!!!!!!")
    browser()
  }else{
    sheets_yearfile <- sheets_yearfile[1:2]
  }
  for(sh in sheets_yearfile){
    data  <- read.xlsx(yearfile, sheetIndex = sh)
    for(sp in SPCS_LIST){
      switch (sp,
        "Sardine"       = spcsrow_key <- "マイワシ漁獲量（㎏）",
        "Anchovy"       = spcsrow_key <- "カタクチイワシ漁獲量（㎏）",
        "RoundHerring"  = spcsrow_key <- "ウルメイワシ漁獲量（㎏）",
        "Mackerels"     = spcsrow_key <- "サバ類漁獲量（㎏）",
        "JackMackerel"  = spcsrow_key <- "マアジ漁獲量（㎏）"
      )
      spcs_row  <- which(data[,COLPOS_SPNAME]==spcsrow_key)
      YEARS     <- YEAR_START:max(data[spcs_row + YEAR_OFFSET, COLPOS_DATSTART:ncol(data)], na.rm = T)
      for(j in 1:length(YEARS)){
        year <- YEARS[j]
        for(i in MONTHS){
          mon             <- MONTHS[i]
          catch_kg        <- data[spcs_row+YEAR_OFFSET+i, COLPOS_DATSTART+j-1]
          out <- rbind(out, cbind(date, year, mon, day, p, sh, sp, catch_kg))
        }
      }
    }
  }
}
out           <- as.data.frame(out)
colnames(out) <- c("Date","Year","Mon","Day", "Prefecture", "FromSheetNamed", "Species","Catch_kg")
out_all       <- rbind(out_all, out)
# head(out)
print(paste("Completed ", p, "!", sep=""))