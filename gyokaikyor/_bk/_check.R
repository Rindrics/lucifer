library(XLConnect)
SPECIES       <- c("maiwashi", "katakuchi")
INDIR         <-"../../Data/_bk" 
DAT_FIRSTROW  <- 4
DAT_LASTROW   <- 20
DAT_FIRSTCOL  <- 2
DAT_LASTCOL   <- 13
PREFEC_COL    <- 1
MONTH_ROW     <- 3
out           <- NULL
for(s in SPECIES){
  indir_h     <- paste(INDIR, list.files(INDIR, pattern = paste(s, "_land_hayashi", sep="")), sep="/")
  indir_y     <- paste(INDIR, list.files(INDIR, pattern = paste(s, "_land_yasuda", sep="")), sep="/")
  dat_hayashi <- read.csv(paste(indir_h, "集計-Table 1.csv", sep="/"), header = F)
  dat_yasuda  <- read.csv(paste(indir_y, "集計-Table 1.csv", sep="/"), header = F)
  for(i in DAT_FIRSTROW:DAT_LASTROW){
    for(j in DAT_FIRSTCOL:DAT_LASTCOL){
      hys_i_j <- as.numeric(gsub(",","",as.character(dat_hayashi[i, j])))
      ysd_i_j <- as.numeric(gsub(",","",as.character(dat_yasuda[i, j])))
      if(hys_i_j != ysd_i_j){
        prefec_name <- as.character(dat_hayashi[i, PREFEC_COL])
        month_name  <- as.character(dat_hayashi[MONTH_ROW, j])
        cat("Data in ", s, " ",prefec_name, month_name, "月 is differ between files!!", "\n")
        out <- rbind(out, cbind(s, prefec_name, month_name))
      }
    }
  }
}
out <- as.data.frame(out)
colnames(out) <- c("Species", "Prefecture", "Month")
write.csv(out, "../../Output/Landing/Diff_hayashi_yasuda_landing.csv", row.names = F)
