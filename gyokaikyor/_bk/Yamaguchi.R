print(paste("Processing catchdata in ", p, "...", sep=""))
COLPOS_MONTHNAME  <- 1
date              <- ""
day               <- ""
out               <- NULL
for(f in fname_prefec){
  yearfile          <- paste(INDIR, FISCAL_YEARS, p, f, sep="/")
  sheets_yearfile   <- excel_sheets(yearfile)
  if(gregexpr("湊市", f)>0){
    rowpos_spcsname   <- 2
    year_sheets       <- na.omit(as.numeric(sheets_yearfile))
    for(y in year_sheets){
      data          <- read.xlsx(yearfile, sheetIndex = as.character(y))
      for(m in MONTHS){
        switch(as.character(m),
          "1"  = monthrow_key <- "１　月　計",
          "2"  = monthrow_key <- "２　月　計",
          "3"  = monthrow_key <- "３　月　計",
          "4"  = monthrow_key <- "４　月　計",
          "5"  = monthrow_key <- "５　月　計",
          "6"  = monthrow_key <- "６　月　計",
          "7"  = monthrow_key <- "７　月　計",
          "8"  = monthrow_key <- "８　月　計",
          "9"  = monthrow_key <- "９　月　計",
          "10" = monthrow_key <- "１０　月　計",
          "11" = monthrow_key <- "１１　月　計",
          "12" = monthrow_key <- "１２　月　計")
        month_row     <- which(data[,COLPOS_MONTHNAME]==monthrow_key)
        for(sp in SPCS_LIST){
          catch_kg <- 0
          switch (sp,
                  "Sardine"      = {spcol_key <- "マイワシ"},
                  "Anchovy"      = spcol_key <- "カタクチ",
                  "RoundHerring" = spcol_key <- "ウルメ",
                  "Mackerels"    = spcol_key <- "サバ",
                  "JackMackerel" = spcol_key <- "マアジ"
          )
          spcol     <- which(data[rowpos_spcsname,]==spcol_key)
          catch_kg  <- data[month_row, spcol]
          sh        <- paste(f, y, sep="_")
          out       <- rbind(out, cbind(date, y, m, day, p, sh, sp, catch_kg))
        }
      }
    }
  }else if(gregexpr("まき網", f)>0){
    rowpos_spcsname           <- 4
    rowpos_year               <- 5
    maybe_nulldata            <- read.xlsx(yearfile, sheetIndex = sheets_yearfile[2])
    if(is.null(maybe_nulldata)==F){
      "Change detected in sheet structure of Yamaguchi Makiami file!!!"
      browser()
    }
    data                      <- read.xlsx(yearfile, sheetIndex = sheets_yearfile[1])
    for(m in MONTHS){
      month_row     <- which(as.character(data[,COLPOS_MONTHNAME])==m)
      for(sp in SPCS_LIST){
        switch (sp,
                "Sardine"      = {spcol_key <- "マイワシ"},
                "Anchovy"      = spcol_key <- "カタクチ",
                "RoundHerring" = spcol_key <- "ウルメ",
                "Mackerels"    = spcol_key <- "サバ",
                "JackMackerel" = spcol_key <- "マアジ"
        )
        for(j in 1:ncol(data)){
          if(is.na(as.character(data[rowpos_spcsname,j]))==F){
            if(gregexpr(spcol_key, as.character(data[rowpos_spcsname,j]))>0){
              spcol <- j
            }
          }
        }
        for(j in c(1, 2)){ #two years
          catch_kg  <- 0
          year      <- as.numeric(sub("H","",as.character(data[rowpos_year, spcol+j])))+1988
          catch_kg  <- data[month_row, spcol+j]
          sh <- paste(f, sheets_yearfile[1], sep="_")
          out       <- rbind(out, cbind(date, y, m, day, p, sh, sp, catch_kg))
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
