AGESHIFT                      <- 1
CODE2FISH                     <- hash(read.csv("/Users/00007738/Data/handmade/fishcode.csv")[,2], read.csv("/Users/00007738/Data/handmade/fishcode.csv")[,1])
CODE2PREFEC                   <- hash(read.csv("/Users/00007738/Data/handmade/prefeccode.csv")[,2], read.csv("/Users/00007738/Data/handmade/prefeccode.csv")[,1])
CODE2UNIT_CATCH               <- hash(read.csv("/Users/00007738/Data/handmade/unitcode_catch.csv")[,1], read.csv("/Users/00007738/Data/handmade/unitcode_catch.csv")[,2])
PREFEC2CODE                   <- hash(read.csv("/Users/00007738/Data/handmade/prefeccode.csv")[,1], read.csv("/Users/00007738/Data/handmade/prefeccode.csv")[,2])
PREFEC_NO_PACIFIC_OCEAN       <- c("Akita","Yamagata","Niigata","Toyama","Ishikawa","Fukui","Kyoto","Hyogo","Tottori","Shimane","Yamaguchi","Saga","Nagasaki","Kumamoto")
FISH2CODE                     <- hash(read.csv("/Users/00007738/Data/handmade/fishcode.csv")[,1],
 read.csv("/Users/00007738/Data/handmade/fishcode.csv")[,2])
COLPOS_Anchovy                <- 34
COLPOS_JackMackerels          <- 37
COLPOS_Mackerels              <- 39
COLPOS_RoundHerring           <- 33
COLPOS_Sardine                <- 32
F_FOOBAR                      <- 1

GRAM_TO_KG                    <- 1/1000
KG_TO_TON                     <- 1/1000
TON_TO_G                      <- 1000000
TON_TO_KG                     <- 1000
MONTHS                        <- 1:12

SUMROW_NOURIN                 <- 10
THOUSAND2MILLION              <- 1/1000
N_TO_N_HUNDREDTHOUSAND        <- 1/100000
N_HUNDREDTHOUSAND_TO_N        <- 100000
N_TO_N_MILLION                <- 1/1000000
YEARSHIFT                     <- 1
FULLWIDMON_HALFWIDMON_TBL     <- hash(c("１","２","３","４","５","６","７","８","９","１０","１１","１２"), MONTHS)
JPYEAR_START_TABLE            <- hash(c("Showa", "Heisei"), c(1925, 1988))
