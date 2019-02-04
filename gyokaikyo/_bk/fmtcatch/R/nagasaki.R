library(readxl)
FULLWIDMON_HALFWIDMON_TBL     <- hash(c("１","２","３","４","５","６","７","８","９","１０","１１","１２"), MONTHS)

MONTHS  <- 1:12
JPYEAR_START_TABLE            <- hash(c("Showa", "Heisei"), c(1925, 1988))
INDIR  <- "../../../OneDrive - 国立研究開発法人 水産研究・教育機構/業務/2017_H29"
OUTDIR  <- "../../../OneDrive - 国立研究開発法人 水産研究・教育機構/業務/2017_H29/output"
TON_TO_KG <- 1000
p     <- "長崎"
print(paste("Processing catchdata in ", p, "...", sep=""))
FISCAL_YEAR_START   <- 2017
SPCS_LIST           <- c("Sardine", "Anchovy", "RoundHerring", "Mackerels", "JackMackerel")
SPCS_LIST_DETAILED  <- c("Sardine", "Anchovy", "RoundHerring", "ChubMackerel", "SouthernMackerel", "JackMackerel")
FISCAL_YEARS        <- paste(FISCAL_YEAR_START, FISCAL_YEAR_START+1, sep="_")
FISCAL_YEARS_LAST   <- paste(FISCAL_YEAR_START-1, FISCAL_YEAR_START, sep="_")

date                <- ""
day                 <- ""
SPNAME_TBL          <- hash(c("マイワシ","カタクチイワシ","ウルメ","マサバ", "ゴマサバ", "マアジ"), SPCS_LIST_DETAILED)
out                 <- NULL
pdir                <- paste(INDIR, p, sep="/")
filelist            <- list.files(pdir, pattern="xls")
filelist            <- filelist[gregexpr("体長組成", filelist)<0]
filelist            <- filelist[gregexpr("漁海況予報", filelist)<0]
filelist            <- filelist[gregexpr("漁況", filelist)<0]
for(f in filelist){
  is_format_good <- F
  if(gregexpr("長崎", f)>0 | gregexpr("橘", f)>0){
    colpos_month_start  <- 3
  }else{
    colpos_month_start  <- 5
  }
  if(gregexpr("九十九島", f)>0){
    if(gregexpr("アシ", f)>0 | gregexpr("サハ", f)>0 | gregexpr("ウルメ", f)>0){
      is_format_good <- T
    }
  }
  if(is_format_good == T){
    colpos_year         <- 1
    infile              <- paste(INDIR, p, f, sep="/")
    data                <- read_excel(infile, sheet = 1)
    for(i in 1:nrow(data)){
      icharacter <- as.character(data[i, colpos_year])
      if(is.na(icharacter)==F){
        if(gregexpr("ウルメ", f)>0){
          colpos_month_start  <- 2
          sp                  <- "RoundHerring"
        }else{
          colpos_month_start  <- 3
          sp                  <- ""
          if(gregexpr("サバ", icharacter)>0){
            sp <- "Mackerels"
          }else if(gregexpr("マアジ", icharacter)>0){
            sp <- "JackMackerel"
          }
        }
        if(sum(unlist(gregexpr("[0-9]",icharacter)))>0){
          if(gregexpr("ウルメ", f)>0){
            year <- as.numeric(icharacter) + JPYEAR_START_TABLE[["Heisei"]]
          }else{
            year <- as.numeric(substr(icharacter, 1, 4))
          }
          for(j in colpos_month_start:(colpos_month_start+length(MONTHS)-1)){
            month     <- j - (colpos_month_start-1)
            sh        <- f
            catch_kg  <- as.numeric(as.character(data[i, j]))*TON_TO_KG
            out       <- rbind(out, cbind(date, year, month, day, p, sh, sp, catch_kg)) 
          }
        }
      }
    }
  }else if(is_format_good == F){
    colpos_spname             <- 1
    col_offset_catch          <- 2
    row_offset_catch          <- 5
    infile                    <- paste(INDIR, p, f, sep="/")
    newdir                    <- sub(".xls","",sub(".xlsx", "", infile))
    if(file.exists(newdir)==F){
      dir.create(newdir)
    }
    sheets_infile             <- excel_sheets(infile)
    for(i in sheets_infile){
      data <- read_excel(infile, sheet=i)
      write.csv(data, paste(newdir, "/",i, ".csv", sep=""), row.names = F)
    }
    for(ym in sheets_infile){
      # print(ym)
      ym_modified               <- gsub(" ","", ym)
      if(nchar(ym_modified)!=15){browser(print(paste("Error found in sheetname of Excel file", f, "->", ym)))}
      # data                      <- read.xlsx(infile, sheetName=ym)
      data                      <- read.csv(paste(newdir, "/", ym, ".csv", sep=""))
      year1                     <- as.numeric(substr(ym_modified, 1, 4))
      year2                     <- as.numeric(substr(ym_modified, 9, 12))
      does_include_year_change  <- year1 != year2
      for(i in 1:nrow(data)){
        icharacter <- as.character(data[i, colpos_spname])
        if(is.na(icharacter)==F){
          if(nchar(icharacter)==1){
            pre_sp <- NULL
            for(ii in i:nrow(data)){
              iicharacter <- as.character(data[ii, colpos_spname])
              if(is.na(iicharacter)==F){
                pre_sp <- paste(pre_sp, iicharacter, sep="")
                if(is.null(SPNAME_TBL[[pre_sp]])==F){
                  # i <- ii
                  break
                }
              }
            }
            icharacter <- pre_sp
          }
          icharacter <- gsub(" ","",gsub("　","", icharacter))
          if(gregexpr("漁況情報",icharacter)[[1]][1]<0 & gregexpr("地名",icharacter)[[1]][1]<0){
            # browser()
            # icharacter
            if(icharacter != "サバ類"){
              sp <- SPNAME_TBL[[icharacter]]
              if(is.null(sp)==F){
                # print(sp)
                monthrow_list <- NULL
                for(k in 1:nrow(data)){
                  kcharacter <- as.character(data[k, colpos_month_start])
                  if(is.na(kcharacter)==F){
                    if(gregexpr("月", kcharacter)>0){
                      monthrow_list <- append(monthrow_list, k)
                    }
                  }
                }
                for(i in monthrow_list){
                  for(j in colpos_month_start:ncol(data)){
                    jcharacter <- as.character(data[i, j])
                    if(is.na(jcharacter)==F){
                      if(gregexpr("の", jcharacter)<0){
                        # browser()
                        if(is.null(FULLWIDMON_HALFWIDMON_TBL[[gsub("　","", sub("月","", jcharacter))]])==F){
                          month <- as.numeric(FULLWIDMON_HALFWIDMON_TBL[[gsub("　","", sub("月","", jcharacter))]])
                        }else{
                          month <- as.numeric(gsub("　","", sub("月","", jcharacter)))
                        }
                        if(does_include_year_change == T){
                          if(month >= 4){
                            year <- year1
                          }else{
                            year <- year2
                          }
                        }else if(does_include_year_change == F){
                          year <- year1
                        }
                        catch_kg <- as.numeric(as.character(data[i+row_offset_catch, j+col_offset_catch]))*TON_TO_KG
                        # cat(year, month, catch_kg, "\n")
                        sh      <- f
                        out     <- rbind(out, cbind(date, year, month, day, p, sh, sp, catch_kg))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
out           <- as.data.frame(out)
colnames(out) <- c("Date","Year","Mon","Day", "Prefecture", "FromSheetNamed", "Species","Catch_kg")
# out_all       <- rbind(out_all, out)
# head(out)
print(paste("Completed ", p, "!", sep=""))
write.csv(out, paste(OUTDIR, "/", p, ".csv", sep = ""))
