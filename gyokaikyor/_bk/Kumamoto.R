ROWPOS_1STYEAR    <- 3
COLPOS_1STMONTH   <- 4
COLPOS_YEAR       <- 3
SPNAME_JP         <- c("マアジ","サバ類","マイワシ","カタクチイワシ","ウルメイワシ" )
SPNAME_TRANSLATE  <- hash(SPNAME_JP, c("JackMackerel","Mackerels","Sardine","Anchovy","RoundHerring" ))
date              <- ""
day               <- ""
out               <- NULL
for(f in fname_prefec){
  if(f==fname_prefec[1]){
    first_month         <- 4
    n_month             <- 12
    includes_yearchange <- T
  }else{
    first_month <- 6
    n_month     <- 7
    includes_yearchange <- F
  }
  infile          <- paste(INDIR, FISCAL_YEARS, p, f, sep="/")
  sheets_infile   <- excel_sheets(infile)
  if(sum(sheets_infile[1:5] == SPNAME_JP)==5){
    sheets_infile <- sheets_infile[1:5]
    for(k in sheets_infile){
       data <- read_excel(infile, sheet=k)
       sp   <- SPNAME_TRANSLATE[[k]]
        for(i in ROWPOS_1STYEAR:nrow(data)){
          yearcell <- as.numeric(as.character(data[i,COLPOS_YEAR]))
          if(is.na(yearcell)==F){
            if(is.numeric(yearcell)==T){
              fiscal_year <- yearcell
              for(j in COLPOS_1STMONTH:(COLPOS_1STMONTH+n_month-1)){
                month     <- first_month + j - COLPOS_1STMONTH
                if(includes_yearchange == T){
                  if(month <= 3){
                    year <- fiscal_year + 1
                  }else{
                    year <- fiscal_year
                  }
                }else{
                  year <- fiscal_year
                }
                catch_kg  <- as.numeric(as.character(data[i, j])) * TON_TO_KG
                sh        <- f
                out       <- rbind(out, cbind(date, year, month, day, p, sh, sp, catch_kg))
              }
            }
          }
        }
    }
  }else{
    browser(print(paste("Change detected in sheetname of", p, f, "!!")))
  }
}
out           <- as.data.frame(out)
colnames(out) <- c("Date","Year","Mon","Day", "Prefecture", "FromSheetNamed", "Species","Catch_kg")
out_all       <- rbind(out_all, out)
# head(out)
print(paste("Completed ", p, "!", sep=""))
