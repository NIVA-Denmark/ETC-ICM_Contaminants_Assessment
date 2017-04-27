rm(list=ls())
library("dplyr")

cat("Starting preprocess.R\n")
#--------------------------------------------------------------------------------------------------------------------------------

datafile <- "data.Rda"
datafolder <- "data_ICES/test/"

variable.Assessment.Unit <- "HELCOM_L4"
variable.Station <- "STATN"

#--------------------------------------------------------------------------------------------------------------------------------
df <- readRDS(file=paste0(datafolder,datafile))


cat("Finished preprocess.R\n")
