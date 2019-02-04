INDIR               <- "../../Data/imported/Gyokaikyo/"
PREFEC_LIST         <- c("Tottori","Shimane","Yamaguchi","Fukuoka","Saga","Nagasaki","Kumamoto","Kagoshima")
PREFEC_SEIKAIBLOCK  <- c("Yamaguchi","Fukuoka","Saga","Nagasaki","Kumamoto","Kagoshima")
FILENAME_TBL        <- read.csv("../../Data/handmade/infiles_gyokaikyo.csv")
FISCAL_YEAR_START   <- 2017
SPCS_LIST           <- c("Sardine", "Anchovy", "RoundHerring", "Mackerels", "JackMackerel")
SPCS_LIST_DETAILED  <- c("Sardine", "Anchovy", "RoundHerring", "ChubMackerel", "SouthernMackerel", "JackMackerel")
FISCAL_YEARS        <- paste(FISCAL_YEAR_START, FISCAL_YEAR_START+1, sep="_")
FISCAL_YEARS_LAST   <- paste(FISCAL_YEAR_START-1, FISCAL_YEAR_START, sep="_")
