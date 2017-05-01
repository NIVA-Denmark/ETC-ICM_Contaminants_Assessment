rm(list=ls())
library("dplyr")

#
# Read text data files from ICES and combine them to a single file
cat("Starting import.R\n")
#--------------------------------------------------------------------------------------------------------------------------------

file.biota <- "DOME_biota_20170428.txt"
file.sediment <- "DOME_sediment_20170429.txt"
file.water <- "DOME_water_20170429.txt"

file.r.biota <- "data_biota.Rda"
file.r.sediment <- "data_sediment.Rda"
file.r.water <- "data_water.Rda"

datafolder <- "data_ICES/"

#--------------------------------------------------------------------------------------------------------------------------------

df.biota<-read.table(paste0(datafolder,file.biota), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.sediment<-read.table(paste0(datafolder,file.sediment), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.water<-read.table(paste0(datafolder,file.water), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)

saveRDS(df.biota,file=paste0(datafolder,file.r.biota))
saveRDS(df.sediment,file=paste0(datafolder,file.r.sediment))
saveRDS(df.water,file=paste0(datafolder,file.r.water))

cat("Finished import.R\n")



