YEAR                <- 2018 # これは資源評価対象年ではなく、業務に取り組んでいる現在年。
YEAR_TO_HEISEI_KEY  <- 12 - 2000
YEAR_HEISEI         <- YEAR + YEAR_TO_HEISEI_KEY
TON_2_KG            <- 1000
BLBIN_mm            <- 5
BLBREAK_MIN_mm      <- 10
BLBREAK_MAX_mm      <- 275 # 大きすぎるように思えるがこれでいい。hist()関数ではデータが階級を越えると都合が悪い。
BLBREAK_MAX_mm_for_Excel_VPAfile <- 160
MAXAGE_ANCHOVY          <- 2
COEF_TON_TO_KILOGRAM    <- 1000
COEF_KILOGRAM_TO_GRAM   <- 1000
COEF_TON_TO_GRAM        <- COEF_TON_TO_KILOGRAM * COEF_KILOGRAM_TO_GRAM
COEF_GRAM_TO_KILOGRAM   <- 1/1000
COEF_KILOGRAM_TO_TON    <- 1/1000
COEF_10TONS_TO_TON      <- 1/10
COEF_GRAM_TO_TON        <- COEF_GRAM_TO_KILOGRAM * COEF_KILOGRAM_TO_TON
HUNDRED_THOUSAND        <- 10^5

INDIR             <- "/Volumes/評価研/資源評価/"
JPYEAR            <- list.files(INDIR, pattern = as.character(YEAR_HEISEI))
SPCSDIR           <- paste(INDIR, JPYEAR, "/カタクチイワシ", sep="")
DATDIR            <- paste(INDIR, JPYEAR, "/各県資料", sep="")
NOURINDIR_PARENT  <- list.files(paste0(INDIR, JPYEAR), pattern = "漁業・養殖業生産統計年報", include.dirs = TRUE, full.names = TRUE)
NOURINDIR         <- paste(NOURINDIR_PARENT, "/H", YEAR_HEISEI-1, "概数", sep="")
NOURINDIR_KAKUTEI <- paste(NOURINDIR_PARENT, "/H", YEAR_HEISEI-2, "確定値", sep="")
GYOKAIKYODIR      <- paste("/Volumes/評価研/漁海況/", YEAR, "年3月", sep="")
GYOKAIKYODIR_LAST <- paste("/Volumes/評価研/漁海況/", YEAR-1, "年10月", sep="")
GYOSEKIDIR        <- paste0("/Volumes/評価研/漁績集計結果（FCC）/浮魚関係/生データ　～", YEAR-1, "/生データ（単位：Ｋｇ） 2011～", YEAR-1)
PACIFIC_SHIPLIST  <- read.csv("_Library/PacificShiplist.csv", header = F, col.names = "ShipName", stringsAsFactors = F)
PREFEC_CODE       <- read.csv("_Library/prefecture_code.csv", fileEncoding = "cp932", stringsAsFactors = F)
CODE_PREFEC_TBL   <- hash(PREFEC_CODE$都道府県支庁コード, PREFEC_CODE$都道府県支庁)
PREFEC_CODE_TBL   <- hash(PREFEC_CODE$都道府県支庁, PREFEC_CODE$都道府県支庁コード)
MONTHS            <- 1:12
OUTDIR            <- paste("output", sep="")
TODOUFUKEN_SOJ    <- c("青森","秋田","山形","新潟","富山","石川","福井","京都","兵庫","鳥取","島根","山口")
TODOUFUKEN_ECS    <- c("福岡","佐賀","長崎","熊本","鹿児島", "西海区")
PREFECTURE_SOJ    <- c("aomori", "akita", "yamagata", "niigata", "toyama", "ishikawa", "fukui", "kyoto", "hyogo", "tottori", "shimane", "yamaguchi")
PREFECTURE_ECS    <- c("fukuoka", "saga", "nagasaki", "kumamoto", "kagoshima", "seikaiku")
TODOUFUKEN_LIST   <- c(TODOUFUKEN_SOJ, TODOUFUKEN_ECS)
PREFECTURE_LIST   <- c(PREFECTURE_SOJ, PREFECTURE_ECS)
PREFEC_TABLE      <- hash(PREFECTURE_LIST,TODOUFUKEN_LIST)
TODOUFUKEN_TABLE  <- hash(TODOUFUKEN_LIST, PREFECTURE_LIST)
BREAKS            <- seq(BLBREAK_MIN_mm, BLBREAK_MAX_mm, by=BLBIN_mm)
CLASS_LEFT        <- BREAKS[1:length(BREAKS)-1]
BREAKS4Excel      <- seq(BLBREAK_MIN_mm, BLBREAK_MAX_mm_for_Excel_VPAfile, BLBIN_mm)
CLASSNAME         <- paste(formatC(CLASS_LEFT, width=3, flag=0), formatC(CLASS_LEFT+BLBIN_mm,width=3,flag=0), sep="-")
CLASSNAME4Excel   <- paste(formatC(BREAKS4Excel[1:(length(BREAKS4Excel)-1)], width=3, flag=0), formatC(BREAKS4Excel[1:(length(BREAKS4Excel)-1)]+BLBIN_mm,width=3,flag=0), sep="-")
AGE_LENGTH_KEY_0  <- read.csv("_Library/ageLengthKey0.csv", row.names = 1)
AGE_LENGTH_KEY_1  <- read.csv("_Library/ageLengthKey1.csv", row.names = 1)
AGE_LENGTH_KEY_2  <- read.csv("_Library/ageLengthKey2.csv", row.names = 1)
COEF_BL2BW        <- unlist(read.csv("_Library/coef_bl2bw.csv", header=F))
