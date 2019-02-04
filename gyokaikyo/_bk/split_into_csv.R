# if(file.exists(CAADIR) == F){
#     dir.create(CAADIR)
#     for(i in CAASHEETS){
#         data <- read_excel(CAAFILE, sheet=i)
#         write.csv(data, paste(CAADIR, "/",i, ".csv", sep=""), row.names = F)
#     }
# }
if(file.exists(VPADIR) == F){
    dir.create(VPADIR)
    for(i in VPASHEETS){
        data <- read_excel(VPAFILE, sheet=i)
        write.csv(data, paste(VPADIR, "/",i, ".csv", sep=""), row.names = F)
    }
}

