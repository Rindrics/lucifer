SPCS                    <- c("Sardine", "Anchovy", "RoundHerring", "Mackerels", "JackMackerels")
SPCS_COL_TBL            <- hash(SPCS, c(COLPOS_Sardine, COLPOS_Anchovy, COLPOS_RoundHerring, COLPOS_Mackerels, COLPOS_JackMackerels))
PREFECNO_TBL            <- hash(read.csv("../../../Data/handmade/NourinPrefecTable.csv")[,1], as.character(read.csv("../../../Data/handmade/NourinPrefecTable.csv")[,2]))
AREA_TBL                <- hash(c("日本海西区","瀬戸内海区","北海道太平洋北区","太平洋北区","太平洋中区","太平洋南区","北海道日本海北区","日本海北区","東シナ海区"), 
                                c("NihonkaiNishi","Setonaikai","HokkaidoTaiheiyoKita","TaiheiyoKita","TaiheiyoNaka","TaiheiyoMinami","HokkaidoNihonkaiKita","NihonkaiKita","Higashishinakai"))
PREFEC_TRANSLATE        <- hash(c("青森", "兵庫", "山口", "福岡"), c("Aomori", "Hyogo", "Yamaguchi", "Fukuoka"))
NOURINDIR               <- "../../../Data/imported/Nourintoukei"
NOURIN_FILES            <- list.files(NOURINDIR, pattern=".+xlsx")
for(f in NOURIN_FILES){
  nourin_file   <- paste(NOURINDIR, f, sep="/")
  nourin_sheets <- excel_sheets(nourin_file)
  nourin_newdir <- sub(".xlsx", "" , nourin_file)
  if(file.exists(nourin_newdir) == F){
    dir.create(nourin_newdir)
    for(i in 1:length(nourin_sheets)){
      data      <- read_excel(nourin_file, sheet=i)
      sheetname <- nourin_sheets[i]
      # print(sheetname)
      write.csv(data, paste(nourin_newdir, "/", formatC(i, width = 2, flag=0), "_", sheetname, ".csv", sep=""), row.names = F)
    }
  }
}
NOURIN_DIRS             <- list.files(NOURINDIR, pattern = ".+\\W$")
out                     <- NULL
for(n in NOURIN_DIRS){
  nourin_dir  <- paste(NOURINDIR, n, sep="/")
  year        <- as.numeric(substr(strsplit(nourin_dir, "H")[[1]][2], 1, 2))+1988
  prefec_csvs <- list.files(nourin_dir)
  if(gregexpr("確定", nourin_dir)>0){
    is_definitive_value   <- T
  }else{
    is_definitive_value   <- F
  }
  if(gregexpr("\\（?\\(?都道府県\\）?\\)?", nourin_dir)>0){
    is_all_prefec         <- T
  }else{
    is_all_prefec         <- F
  }
  for(sp in 1:length(SPCS)){
    spcs_name <- SPCS[sp]
    for(i in 1:length(prefec_csvs)){
      filename        <- prefec_csvs[i]
      if(is_all_prefec == T){
        prefec_name   <- PREFECNO_TBL[[as.character(i)]]
        area          <- ""
      }else{
        if(gregexpr("[\\（,\\(]大海区[\\）,\\)]", nourin_dir)>0){
          prefec_name <- "Daikaiku"
          area        <- AREA_TBL[[sub(".csv","",strsplit(filename, "_")[[1]][2])]]
        }else{
          prefec_name <- PREFEC_TRANSLATE[[substr(filename, 4, 5)]]
          area        <- AREA_TBL[[sub(".csv","",strsplit(filename, "_")[[1]][3])]]
        }
      }
      data                              <- read.csv(paste(nourin_dir, filename, sep="/"), header = F)
      j                                 <- SPCS_COL_TBL[[spcs_name]]
      if(as.character(data[SUMROW_ROURIN, j])=="-"){
        # browser()
        # cat(sp, filename, "\n")
        catch     <- NA
        property  <- "-"
      }else if(as.character(data[SUMROW_ROURIN, j])=="x"){
        catch     <- NA
        property  <- "x"
      }else{
        catch     <- as.numeric(as.character(data[SUMROW_ROURIN, j]))
        property  <- ""
        if(is.na(catch)==T){
          browser()
          cat(spcs_name, filename, catch, "\n")
        }
      }
      out <- rbind(out, cbind(year, is_definitive_value, spcs_name, is_all_prefec, prefec_name, area, catch, property))
    }
  }
}
out           <- as.data.frame(out)
colnames(out) <- c("Year", "IsDefinitiveValue", "Species", "IsTodoufukenbetsu", "Prefecture", "Area", "Catch_ton", "Property")
unique(out[,"Area"])
write.csv(out, "../../../Output/Landing/Nourintoukei.csv", row.names = F)
rm(sp)
rm(SPCS)
NOURINTOUKEI                <- read.csv("../../../Output/Landing/Nourintoukei.csv")
NOURINTOUKEI[,"Species"]    <- as.character(NOURINTOUKEI[,"Species"])
NOURINTOUKEI[,"Prefecture"] <- as.character(NOURINTOUKEI[,"Prefecture"])
NOURINTOUKEI[,"Area"]       <- as.character(NOURINTOUKEI[,"Area"])
NOURINTOUKEI[,"Property"]   <- as.character(NOURINTOUKEI[,"Property"])
