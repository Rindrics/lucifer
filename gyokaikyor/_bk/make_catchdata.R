FILENAME_TBL[,"Prefecture"] <- as.character(FILENAME_TBL[,"Prefecture"])
FILENAME_TBL[,"Filename"]   <- as.character(FILENAME_TBL[,"Filename"])
out_all                     <- NULL
for(p in PREFEC_LIST){
  fname_prefec  <- as.character(FILENAME_TBL[FILENAME_TBL[,"Prefecture"]==p,"Filename"])
  switch (p,
    "Tottori"    = source("calcLanding/Tottori.R"),
    "Shimane"    = source("calcLanding/Shimane.R"),
    "Yamaguchi"  = source("calcLanding/Yamaguchi.R"),
    "Fukuoka"    = source("calcLanding/Fukuoka.R"),
    "Saga"       = source("calcLanding/Saga.R"),
    "Nagasaki"   = source("calcLanding/Nagasaki.R"),
    "Kumamoto"   = source("calcLanding/Kumamoto.R"),
    "Kagoshima"  = source("calcLanding/Kagoshima.R")
  )
}
write.csv(out_all, "../../Output/Gyokaikyo/catch_rawdata.csv", row.names = F)
# RAWDATA  <- read.csv("../../Output/Gyokaikyo/catch_rawdata.csv", stringsAsFactors = F)
# head(RAWDATA)
# tail(RAWDATA)
# length(unique(RAWDATA$Prefecture))
