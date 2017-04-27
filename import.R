rm(list=ls())
library("dplyr")

#
# Read text data files from ICES and combine them to a single file
cat("Starting import.R\n")
#--------------------------------------------------------------------------------------------------------------------------------

file.biota <- "HELCOM_HZ_biota_20170203.txt"
file.sediment <- "HELCOM_HZ_sediment_20170123.txt"
file.water <- "HELCOM_HZ_water_20170118.txt"

file.combined <- "data.Rda"
datafolder <- "data_ICES/test/"

variable.Assessment.Unit <- "HELCOM_L4"
variable.Station <- "STATN"

#--------------------------------------------------------------------------------------------------------------------------------

df.biota<-read.table(paste0(datafolder,file.biota), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.sediment<-read.table(paste0(datafolder,file.sediment), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.water<-read.table(paste0(datafolder,file.water), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)

df.biota$Category <- "Biota"
df.sediment$Category <- "Sediment"
df.water$Category <- "Water"

df <- bind_rows(df.biota,df.sediment,df.water)

saveRDS(df,file=paste0(datafolder,file.combined))

cat("Finished import.R\n")
#--------------------------------------------------------------------------------------------------------------------------------


