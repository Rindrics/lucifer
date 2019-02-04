print(paste("Processing catchdata in ", p, "...", sep=""))
out <- NULL
for(f in fname_prefec){
  yearfile          <- paste(INDIR, FISCAL_YEARS, p, f, sep="/")
  sheets_yearfile   <- excel_sheets(yearfile)
  if(sum(c("大型", "小型") %in% sheets_yearfile)!=2){
    print("Change in Excel sheetname was found in Tottori!!!!!!")
    browser()
  }else{
    sheets_yearfile <- sheets_yearfile[1:2]
  }
  for(i in sheets_yearfile){
    data <- read.xlsx(yearfile, sheetIndex = i)
    data[,"Date"] <- as.Date(paste(data[,"年"], data[,"月"], data[,"日"],sep="-"))
    for(sp in SPCS_LIST){
      catch_kg <- 0
      switch (sp,
        "Sardine"      = catch_kg <- data[,"マイワシ小計"],
        "Anchovy"      = catch_kg <- data[,"カタクチ小計"],
        "RoundHerring" = catch_kg <- data[,"ウルメ小計"],
        "Mackerels"    = catch_kg <- data[,"マサバ小計"], #maybe not meant to "Chub mackerel"
        "JackMackerel" = catch_kg <- data[,"マアジ小計"]
      )
      out <- rbind(out, cbind(data[,"Date"], data[,"年"], data[,"月"], data[,"日"], p, i, sp, catch_kg))
    }
  }
}
out           <- as.data.frame(out)
colnames(out) <- c("Date","Year","Mon","Day", "Prefecture", "FromSheetNamed", "Species","Catch_kg")
out[,"Date"]  <- as.character(as.Date(as.numeric(as.character(out[,"Date"])), origin="1970-01-01"))
out_all       <- rbind(out_all, out)
# head(out)
print(paste("Completed ", p, "!", sep=""))
