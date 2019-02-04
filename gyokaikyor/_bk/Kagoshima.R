SPNAME_JP           <- hash(c("マイワ","カタク","ウルメ","サバ類","マアジ"),c("Sardine", "Anchovy", "RoundHerring", "Mackerels", "JackMackerel"))
COLPOS_MONTH        <- 1
COLOFFSET_SUM       <- 4
date                <- ""
day                 <- ""
out                 <- NULL
infile              <- paste(INDIR, FISCAL_YEARS, p, fname_prefec, sep="/")
sheets_infile       <- excel_sheets(infile)
if(sum(sheets_infile == c("４港計","中まき４港計","阿久根","枕崎(旧式）","内之浦","山川","阿久根棒受","内之浦棒受","新枕崎","中まき送付用"))!=10){
  browser(print(paste("Change detected in", p, "file !!!")))
}else{
  sheets_infile <- c("４港計","阿久根棒受","内之浦棒受" )
}
ENDROW_TBL            <- hash(sheets_infile, c(16,14,14))
ROWOFFSET_MON_TBL     <- hash(sheets_infile, c(4,2,2))
SPNAMEROW_TBL         <- hash(sheets_infile, c(3,2,2))
for(sh in sheets_infile){
  data            <- read.xlsx(infile, sheetIndex = sh, header = F, endRow = ENDROW_TBL[[sh]])
  rowoffset_mon   <- ROWOFFSET_MON_TBL[[sh]]
  for(j in 1:ncol(data)){
    pre_spname    <- as.character(data[SPNAMEROW_TBL[[sh]], j])
    pre_spname    <- gsub("　", "", pre_spname)
    spname_3char  <- substr(pre_spname,1,3)
    if(is.null(SPNAME_JP[[spname_3char]])==F){
      if(spname_3char %in% c("マアジ", "サバ類")){
        if(gregexpr("計", pre_spname)>0){
          sp <- SPNAME_JP[[spname_3char]]
          for(i in 1:length(MONTHS)){
            month <- i + 3*(i<=9) -9*(i>=10)
            # print(month)
            catch_kg <- as.numeric(as.character(data[i+rowoffset_mon, j]))
            if(month<=3){
              year <- FISCAL_YEAR_START+1
            }else if(month>=4){
              year <- FISCAL_YEAR_START
            }
            out     <- rbind(out, cbind(date, year, month, day, p, sh, sp, catch_kg))
          }
        }
      }else{
        sp <- SPNAME_JP[[spname_3char]]
        for(i in 1:length(MONTHS)){
          month <- i + 3*(i<=9) -9*(i>=10)
          # print(month)
          catch_kg <- as.numeric(as.character(data[i+rowoffset_mon, j]))
          if(month<=3){
            year <- FISCAL_YEAR_START+1
          }else if(month>=4){
            year <- FISCAL_YEAR_START
          }
          out     <- rbind(out, cbind(date, year, month, day, p, sh, sp, catch_kg))
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
