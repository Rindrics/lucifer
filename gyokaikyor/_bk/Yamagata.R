INDIR             <- paste("../../../Data/imported/Prefecture/", p, sep="")
list.files(INDIR)
SPECIES_PREFEC    <- "Mackerels"
SPNAME_TRANSRATE  <- hash(SPECIES_PREFEC, c("^マサハ."))
YEARROW           <- 4
MONTHCOL          <- 1

EXCELFNAME      <- as.character(FILENAME_TBL[[p]])
TEMP            <- loadWorkbook(paste(INDIR, EXCELFNAME, sep="/"))
DATA_ORG        <- readWorksheet(TEMP, sheet=getSheets(TEMP))
NEWDIR          <- paste(INDIR, sub(".xls","",sub(".xlsx", "", EXCELFNAME)), sep="/")
ROWSKIP_TBL     <- hash(TARGET_FILES, c(2, 4))
if(file.exists(NEWDIR)==F){
    dir.create(NEWDIR)
}
for(i in 1:length(DATA_ORG)){
    data <- DATA_ORG[[i]]
    write.csv(data, paste(NEWDIR, "/",names(DATA_ORG[i]), ".csv", sep=""), row.names = F)
}
for(spp in SPECIES_PREFEC){
  out             <- NULL
  spname_jp       <- SPNAME_TRANSRATE[[spp]]
  data            <- read.csv(paste(NEWDIR, list.files(NEWDIR, pattern = spname_jp), sep="/"), header=F)
  data[,MONTHCOL] <- as.character(data[,MONTHCOL])
  for(j in 1:ncol(data)){
    if(data[YEARROW, j]==YEAR){break()}
  }
  datacol   <- j 
  for(m in MONTHS){
    landing <- 0
    for(i in (YEARROW+1):nrow(data)){
      mon_cell  <- unlist(strsplit(data[i, MONTHCOL], "集"))
      # cat(mon_cell, i, "\n")
      mon       <- as.numeric(as.character(mon_cell[1]))
      if(is.na(mon)==F){
        if(mon == m){
          if(length(mon_cell)==2){
            # browser()
            landing <- sub(",", "", as.character(data[i, datacol]))
            if(is.na(landing)==F){
                if(landing==""){
                  landing <- 0
                }else{
                  landing <- as.numeric(landing)
                }
                break()
            }else{
                landing <- 0
            }
          }
        }else{
          if(mon > m){
            landing <- 0
            break()
          }
        }
      }
    }
    out <- rbind(out, cbind(m, landing))
    write.table(t(out), paste("../../../Output/Landing/", p, "_", spp, "_", YEAR, ".csv", sep=""), row.names=F, col.names=F, sep=",")
  }
}