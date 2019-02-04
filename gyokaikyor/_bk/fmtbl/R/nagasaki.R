source("./_packages.R")
source("./_constants.R")
prefecture      <- "nagasaki"
species         <- "カタクチ"
todoufuken      <- paste(PREFEC_TABLE[[prefecture]], "県", sep="")
years           <- (YEAR-1):YEAR
month_gyokaikyo <- c(3, 10)
yearmon <- NULL
for(y in years){
  for(m in month_gyokaikyo){
    yearmon <- append(yearmon, paste0(y, "年", m, "月"))
  }
}
dirlist   <- paste0("/Volumes/評価研/漁海況/", yearmon, "/各県資料/", todoufuken)

monthrow        <- 3
classstart_col  <- 2
out             <- NULL
colname_monyear <- NULL
for(dir in dirlist){
  blhist <- NULL
  infile <- list.files(dir, pattern="^[^~].+体長組成", full.names = TRUE)
  if (length(infile) > 0) {
    sheets <- excel_sheets(infile)
    sheet  <- sheets[which(gregexpr(species, sheets)>0)]
    data   <- read_xls(infile, sheet=sheet)
    for(i in 1:nrow(data)){
        i_class_start <- as.numeric(as.character(data[i, classstart_col]))
        if(is.na(i_class_start) == FALSE){
            if(i_class_start == BLBREAK_MIN_mm){
                startrow <- i
                break
            }
        }
    }
    endrow       <- startrow + length(CLASSNAME)-1
    monthcollist <- which(gregexpr("月", data[monthrow,]) > 0)
    for(j in monthcollist){
        month      <- as.numeric(sub("月", "", data[monthrow, j]))
        ycandidate <- data[monthrow-1, j]
        if (is.na(ycandidate) == FALSE){
          year <- as.numeric(substr(ycandidate, 1, 4))
        }
        blhist <- cbind(blhist, parse_number(parse_character(unlist(data[startrow:endrow, j]))))
        colname_monyear <- append(colname_monyear, paste0(month.abb[month], year))
    }
    out <- cbind(out, blhist)
  }
}
out <- as.data.frame(out)

colnames(out) <- colname_monyear
rownames(out) <- CLASSNAME
outfname    <- paste("../", OUTDIR, "/体長組成_", PREFEC_TABLE[[prefecture]], ".csv", sep="")
out4stac    <- select(out, paste0(month.abb, YEAR-1))[rownames(out) %in% CLASSNAME4Excel,]
out4gkk     <- select(out, paste0(month.abb[1:8], YEAR))[rownames(out) %in% CLASSNAME4Excel,]
out4stac[is.na(out4stac)] <- 0
out4gkk[is.na(out4gkk)]   <- 0
write.csv(out4gkk, outfname)

print(out2)
