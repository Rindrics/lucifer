print(paste("Processing catchdata in ", p, "...", sep=""))
COLPOS_YEAR         <- 2
COLPOS_MONTH        <- 3
date                <- ""
day                 <- ""
out                 <- NULL
for(f in fname_prefec){
  yearfile          <- paste(INDIR, FISCAL_YEARS, p, f, sep="/")
  sheets_yearfile   <- excel_sheets(yearfile)
  if(sum(sheets_yearfile[1:3]==c("まき網 ", "棒受網", "その他漁業"))!=3){
    print("Change detected in the Fukuoka file!!")
    browser()
  }else{
    sheets_yearfile <- sheets_yearfile[1:3]
  }
  for(sh in sheets_yearfile){
    switch(sh,
           "まき網 "={
             data              <- read.xlsx2(yearfile, sheetName = sh)
             rowpos_year_start <- 3
           },
           "棒受網"={
             data              <- read.xlsx(yearfile, sheetName = sh)
             rowpos_year_start <- 5       
           },
           "その他漁業"={
             data              <- read.xlsx(yearfile, sheetName = sh)
             rowpos_year_start <- 3       
           })
    data              <- data[data[,COLPOS_MONTH]!="KEI",]
    for(i in rowpos_year_start:nrow(data)){
      if(is.na(as.character(data[i, COLPOS_YEAR]))==T){
        break
      }else if(as.character(data[i, COLPOS_YEAR])==""){
        break
      }
    }
    endrow              <- i - 1
    data                <- data[1:endrow,]
    year_name           <- "Showa"
    data[,"YearName"]   <- ""
    data[,"Year"]       <- 0
    for(i in (rowpos_year_start+1):nrow(data)){
      data[i-1,"YearName"]  <- year_name
      data[i-1, "Year"]     <- as.numeric(as.character(data[i-1, COLPOS_YEAR])) + JPYEAR_START_TABLE[[data[i-1, "YearName"]]]
      if(as.numeric(as.character(data[i, COLPOS_YEAR]))==1 & as.numeric(as.character(data[i-1, COLPOS_YEAR]))==63){
        year_name <- "Heisei"
      }
      data[i,"YearName"]  <- year_name
      data[i, "Year"]     <- as.numeric(as.character(data[i, COLPOS_YEAR])) + JPYEAR_START_TABLE[[data[i, "YearName"]]]
    }
    if(sh=="まき網 "){
      for(sp in SPCS_LIST_DETAILED){
        catch_kg <- 0
        switch (sp,
                "Sardine"          = spcol <- 19,
                "Anchovy"          = spcol <- 21,
                "RoundHerring"     = spcol <- 20,
                "ChubMackerel"     = spcol <- 12,
                "SouthernMackerel" = spcol <- 11,
                "JackMackerel"     = spcol <- 5
        )
        year      <- data[rowpos_year_start:nrow(data),"Year"]
        month     <- as.character(data[rowpos_year_start:nrow(data),COLPOS_MONTH])
        catch_kg  <- as.character(data[rowpos_year_start:nrow(data),spcol])
        out       <- rbind(out, cbind(date, year, month, day, p, sh, sp, catch_kg))
      }
    }else if(sh=="棒受網"){
      for(sp in SPCS_LIST_DETAILED){
        if(sp != "SouthernMackerel"){
          catch_kg <- 0
          switch (sp,
                  "Sardine"          = spcol <- 18,
                  "Anchovy"          = spcol <- 20,
                  "RoundHerring"     = spcol <- 19,
                  "ChubMackerel"     = spcol <- 13,
                  "JackMackerel"     = spcol <- 6
          )
          year      <- data[rowpos_year_start:nrow(data),"Year"]
          month     <- as.character(data[rowpos_year_start:nrow(data),COLPOS_MONTH])
          catch_kg  <- as.character(data[rowpos_year_start:nrow(data),spcol])
          out       <- rbind(out, cbind(date, year, month, day, p, sh, sp, catch_kg))
        }
      }
    }else if(sh=="その他漁業"){
      for(sp in SPCS_LIST_DETAILED){
        catch_kg <- 0
        switch (sp,
                "Sardine"          = spcol <- 19,
                "Anchovy"          = spcol <- 21,
                "RoundHerring"     = spcol <- 20,
                "ChubMackerel"     = spcol <- 12,
                "SouthernMackerel" = spcol <- 11,
                "JackMackerel"     = spcol <- 5
        )
        year      <- data[rowpos_year_start:nrow(data),"Year"]
        month     <- as.character(data[rowpos_year_start:nrow(data),COLPOS_MONTH])
        catch_kg  <- as.character(data[rowpos_year_start:nrow(data),spcol])
        out       <- rbind(out, cbind(date, year, month, day, p, sh, sp, catch_kg))
      }
    }
  }
}
out           <- as.data.frame(out)
colnames(out) <- c("Date","Year","Mon","Day", "Prefecture", "FromSheetNamed", "Species","Catch_kg")
out_all       <- rbind(out_all, out)
# head(out)
print(paste("Completed ", p, "!", sep=""))
