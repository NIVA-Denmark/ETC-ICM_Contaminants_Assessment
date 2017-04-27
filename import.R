# import data from ICES
rm(list=ls())
library("dplyr")
library("ggplot2")

source("options.R")

#--------------------------------------

df.biota<-read.table(paste0(datafolder,file.biota), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.sediment<-read.table(paste0(datafolder,file.sediment), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.water<-read.table(paste0(datafolder,file.water), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)

df<-filter(df.sediment,PARAM=="AL",MUNIT %in% c("%","mg/kg","g/kg","ug/kg"))

distinct(df,MUNIT)

df$multiplier<-ifelse(df$MUNIT=="g/kg",0.1,
                      ifelse(df$MUNIT=="mg/kg",1e-4,
                             ifelse(df$MUNIT=="ug/kg",1e-7,1)))

df$Value<-df$Value*df$multiplier

df$Value<-df$Value*ifelse(df$Value<0.02,1000,1)

df$MUNIT<-"%"

n1<-data.frame(names(df.biota))
n2<-data.frame(names(df.sediment))
n3<-data.frame(names(df.water))
names(n1)<-c("biota")
names(n2)<-c("sediment")
names(n3)<-c("water")

n2$biota<-n2$sediment
n3$biota<-n3$water

n<-left_join(n1,n2)
n<-left_join(n,n3)

test <- df.biota %>%
  group_by_(variable.Assessment.Unit,variable.Station) %>%
  ungroup %>%
  group_by_(variable.Assessment.Unit) %>%
  summarise(n=n())
