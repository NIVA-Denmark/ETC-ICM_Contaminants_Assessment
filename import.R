rm(list=ls())
library("dplyr")

#
# Read text data files from ICES and combine them to a single file
cat("Starting import.R\n")
#--------------------------------------------------------------------------------------------------------------------------------

file.biota.1 <- "DOME_biota_20170428.txt"
file.sediment.1 <- "DOME_sediment_20170429.txt"
file.water.1 <- "DOME_water_20170429.txt"

file.biota.2 <-"EIONET_biota_20170509.txt"
file.water.2 <- "EIONET_seawater_20170509.txt"
file.sediment.2 <- "EIONET_sediment_20170509.txt"

file.biota.3 <-"portugal biota.txt"
file.water.3 <- "portugal water.txt"
file.sediment.3 <- "portugal sediment.txt"

file.r.biota.1 <- "data_biota_DOME.Rda"
file.r.sediment.1 <- "data_sediment_DOME.Rda"
file.r.water.1 <- "data_water_DOME.Rda"

file.r.biota.2 <- "data_biota_EIONET.Rda"
file.r.sediment.2 <- "data_sediment_EIONET.Rda"
file.r.water.2 <- "data_water_EIONET.Rda"

file.r.biota.3 <- "data_biota_PT.Rda"
file.r.sediment.3 <- "data_sediment_PT.Rda"
file.r.water.3 <- "data_water_PT.Rda"

datafolder <- "data_ICES/"
datafolder3 <- "data_PT/"

#--------------------------------------------------------------------------------------------------------------------------------

df.biota.1<-read.table(paste0(datafolder,file.biota.1), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.sediment.1<-read.table(paste0(datafolder,file.sediment.1), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.water.1<-read.table(paste0(datafolder,file.water.1), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)

df.biota.2<-read.table(paste0(datafolder,file.biota.2), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.sediment.2<-read.table(paste0(datafolder,file.sediment.2), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.water.2<-read.table(paste0(datafolder,file.water.2), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)

df.biota.3<-read.table(paste0(datafolder3,file.biota.3), quote="",sep="\t", header=TRUE, fileEncoding="UTF-8", stringsAsFactors=FALSE)
df.sediment.3<-read.table(paste0(datafolder3,file.sediment.3), quote="",sep="\t", header=TRUE, fileEncoding="UTF-8", stringsAsFactors=FALSE)
df.water.3<-read.table(paste0(datafolder3,file.water.3), quote="",sep="\t", header=TRUE, fileEncoding="UTF-8", stringsAsFactors=FALSE)

# df.biota <- bind_rows(df.biota.1,df.biota.2)
# df.sediment <- bind_rows(df.sediment.1,df.sediment.2)
# df.water <- bind_rows(df.water.1,df.water.2)

saveRDS(df.biota.1,file=paste0(datafolder,file.r.biota.1))
saveRDS(df.sediment.1,file=paste0(datafolder,file.r.sediment.1))
saveRDS(df.water.1,file=paste0(datafolder,file.r.water.1))
saveRDS(df.biota.2,file=paste0(datafolder,file.r.biota.2))
saveRDS(df.sediment.2,file=paste0(datafolder,file.r.sediment.2))
saveRDS(df.water.2,file=paste0(datafolder,file.r.water.2))
saveRDS(df.biota.3,file=paste0(datafolder3,file.r.biota.3))
saveRDS(df.sediment.3,file=paste0(datafolder3,file.r.sediment.3))
saveRDS(df.water.3,file=paste0(datafolder3,file.r.water.3))

cat("Finished import.R\n")



