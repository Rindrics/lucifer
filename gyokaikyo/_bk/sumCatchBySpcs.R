DATA      <- read.csv("../../Output/Gyokaikyo/catch_rawdata.csv",stringsAsFactors = F)
YEARLIST  <- unique(DATA$Year); YEARLIST <- YEARLIST[order(YEARLIST)]
for(sp in SPCS_LIST){
  if(sp %in% c("Sardine", "Anchovy", "RoundHerring")){
    out     <- NULL
    spdata  <- DATA[DATA[,"Species"]==sp,]
    for(p in PREFEC_LIST){
      pdata   <- spdata[spdata[,"Prefecture"]==p,]
      pdata2  <- pdata
      switch(sp,
             # "Sardine"      = {switch(p,
                                      # "Tottori"   ={},
                                      # "Shimane"   ={},
                                      # "Yamaguchi" ={},
                                      # "Fukuoka"   ={},
                                      # "Saga"      ={},
                                      # "Nagasaki"  ={},
                                      # "Kumamoto"  ={},
                                      # "Kagoshima" ={})
                               # },
             "Anchovy"      = {switch(p,
                                      # "Tottori"   ={},
                                      # "Shimane"   ={},
                                      "Yamaguchi" = pdata2 <- pdata[gregexpr("まき網",pdata[,"FromSheetNamed"])<0,],
                                      "Fukuoka"   = pdata2 <- pdata[gregexpr("その他",pdata[,"FromSheetNamed"])<0,]
                                      # "Saga"      ={},
                                      # "Nagasaki"  ={},
                                      # "Kumamoto"  ={},
                                      # "Kagoshima" ={}
                                      )
                               },
             "RoundHerring" = {switch(p,
                                      # "Tottori"   ={},
                                      # "Shimane"   ={},
                                      "Yamaguchi" = pdata2 <- pdata[gregexpr("まき網",pdata[,"FromSheetNamed"])<0,],
                                      "Fukuoka"   = pdata2 <- pdata[gregexpr("その他",pdata[,"FromSheetNamed"])<0,]
                                      # "Saga"      ={},
                                      # "Nagasaki"  ={},
                                      # "Kumamoto"  ={},
                                      # "Kagoshima" ={}
                                      )
                               }
             )
      for(y in YEARLIST){
        for(m in MONTHS){
          ymdata <- pdata2[pdata2[,"Year"]==y & pdata2[,"Mon"]==m,]
          if(nrow(ymdata)>0){
            catch_kg <- sum(ymdata$Catch_kg, na.rm=T)
            out <- rbind(out, cbind(y, m, p, sp, catch_kg))
          }
        }
      }
    }
    out           <- as.data.frame(out)
    colnames(out) <- c("Year","Mon","Prefecture","Species","Catch_kg")
    write.csv(out, paste("../../Output/Gyokaikyo/catch_", sp, ".csv", sep=""), row.names = F)
  }
}
