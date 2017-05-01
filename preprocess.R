rm(list=ls())
library("dplyr")
library("tidyr")

cat("Starting preprocess.R\n")
#--------------------------------------------------------------------------------------------------------------------------------
file.r.biota <- "data_biota.Rda"
file.r.sediment <- "data_sediment.Rda"
file.r.water <- "data_water.Rda"
datafolder <- "data_ICES/"
thrshfolder <- "thresholds/"

df.biota <- readRDS(paste0(datafolder,file.r.biota))
df.sediment <- readRDS(paste0(datafolder,file.r.sediment))
df.water <- readRDS(paste0(datafolder,file.r.water))


df.conversion<-read.table(paste0(datafolder,"species_avg_lipid_drywt_OSPAR.txt"), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.species<-read.table(paste0(datafolder,"species_type.txt"), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.params<-read.table(paste0(datafolder,"PARAM.txt"), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)

#------------------ process thresholds  -----------------------------------------
df.thrsh <- read.table(paste0(thrshfolder,"thresholds.txt"), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.groups <- read.table(paste0(thrshfolder,"groups.txt"), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.unit.factor <- read.table(paste0(thrshfolder,"unit_factor.txt"), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)

df.groups <- select(df.groups,PARAM,GROUP,Multiplier,Required)

df.thrsh <- filter(df.thrsh,!PARAM=="")
df.thrsh.1 <- filter(df.thrsh,GROUP==1)
df.thrsh.2 <- filter(df.thrsh,is.na(GROUP))
df.thrsh.1 <-left_join(df.thrsh.1,df.groups,by=c("PARAM"="GROUP"))
df.thrsh.1$GROUP <- df.thrsh.1$PARAM
df.thrsh.1$PARAM <- df.thrsh.1$PARAM.y
df.thrsh.1$PARAM.y <- NULL
df.thrsh.2$GROUP <- df.thrsh.2$PARAM
df.thrsh.2$Multiplier <- 1
df.thrsh.2$Required <- NA
df.thrsh <- rbind(df.thrsh.1,df.thrsh.2)
rm(list=c("df.thrsh.1","df.thrsh.2"))


# where threshold is specified for both shellfish and fish, then make a copy foreach of them
df.thrsh.1 <- filter(df.thrsh,Biota.Type!="Both")
df.thrsh.2 <- filter(df.thrsh,Biota.Type=="Both")
df.thrsh.2$Biota.Type <- "Fish"
df.thrsh <- rbind(df.thrsh.1,df.thrsh.2)
df.thrsh.2$Biota.Type <- "Shellfish"
df.thrsh <- rbind(df.thrsh,df.thrsh.2)
df.thrsh <- arrange(df.thrsh, Category, ID)

rm(list=c("df.thrsh.1","df.thrsh.2"))

#------------------ process biota -----------------------------------------
# LNMEA – Length mean
# DRYWT% - Dryweight percent
# EXLIP% - Extractable lipid percent
# FATWT% - Fat weight percent
# LIPIDWT%
# priority 1. FATWT%, 2. LIPIDWT%, 3. EXLIP%

# Cleaning
#incorrect unit used for some data - I don't know what the correct one is, so delete these lines
df.biota <- filter(df.biota,MUNIT!="umol/min/mg protein")

# replace "ug Sn/kg" with "ug/kg"
df.biota$MUNIT <- ifelse(df.biota$MUNIT=="ug Sn/kg","ug/kg",df.biota$MUNIT)
df.biota$MATRX<-ifelse(df.biota$MATRX=="MU&EP","MU",df.biota$MATRX)
df.biota$Species<-ifelse(df.biota$Species=="Clupea harengus membras","Clupea harengus",df.biota$Species)

df.biota <- left_join(df.biota,df.species,by=c("Species"="Species"))
df.biota <- filter(df.biota,Type %in% c("Fish","Shellfish"))

df.biota$Value <- df.biota$Value*ifelse(df.biota$QFLAG=="<",0.5,1)
df.biota <- select(df.biota,-c(MPROG,RLABO,ALABO,VFLAG,DETLI,METCU,STATN,
                               Latitude,Longitude,NOINP,UNCRT,tblSpotID,
                               tblUploadID,tblAnalysisID,QFLAG,
                               LMQNT))

df.biota.norm <- df.biota %>%
  filter(PARAM %in% c("LNMEA","DRYWT%","EXLIP%","FATWT%","LIPIDWT%")) %>%
  group_by(tblBioID,tblSampleID,Species,PARAM,MATRX,BASIS,MUNIT) %>%
  summarize(Value=mean(Value,na.rm=TRUE)) %>%
  ungroup()


df.biota.DRYWT <- df.biota.norm %>%
  filter(PARAM == "DRYWT%") %>%
  select(tblBioID,tblSampleID,Species,MATRX,DRYWT=Value)
df.biota.EXLIP <- df.biota.norm %>%
  filter(PARAM == "EXLIP%") %>%
  select(tblBioID,tblSampleID,Species,MATRX,EXLIP=Value)
df.biota.FATWT <- df.biota.norm %>%
  filter(PARAM == "FATWT%") %>%
  select(tblBioID,tblSampleID,Species,MATRX,FATWT=Value)
df.biota.LIPIDWT <- df.biota.norm %>%
  filter(PARAM == "LIPIDWT%") %>%
  select(tblBioID,tblSampleID,Species,MATRX,LIPIDWT=Value)

df.biota.norm<-full_join(df.biota.DRYWT,df.biota.FATWT,by=c("tblBioID","tblSampleID","Species","MATRX"))
df.biota.norm<-full_join(df.biota.norm,df.biota.LIPIDWT,by=c("tblBioID","tblSampleID","Species","MATRX"))
df.biota.norm<-full_join(df.biota.norm,df.biota.EXLIP,by=c("tblBioID","tblSampleID","Species","MATRX"))

df.biota.norm$FATWT <- ifelse(is.na(df.biota.norm$FATWT),
                                 ifelse(is.na(df.biota.norm$LIPIDWT),
                                        df.biota.norm$EXLIP,
                                        df.biota.norm$LIPIDWT),
                                df.biota.norm$FATWT)
df.biota.norm <- select(df.biota.norm,-c(EXLIP,LIPIDWT))
                               
df.biota <- filter(df.biota,!PARAM %in% c("LNMEA","DRYWT%","EXLIP%","FATWT%","LIPIDWT%"))
df.biota <-left_join(df.biota, df.biota.norm)

df.thrsh.be <- filter(df.thrsh,Category=="BioEffects") #Threshold.Species,
df.thrsh <- select(df.thrsh,PARAM,GROUP,Category,Biota.Type,Threshold.Unit,Threshold.BASIS,Threshold.Tissue,Threshold.Value,Multiplier,Required)

rm(list=c("df.biota.DRYWT","df.biota.FATWT","df.biota.EXLIP","df.biota.LIPIDWT","df.biota.norm"))

df.bioeffects<-filter(df.biota,PARAM %in% c("VDSI","MNC","LMD"))
df.biota<-filter(df.biota,!PARAM %in% c("VDSI","MNC","LMD"))


# ------------------------ bioeffects -----------------------------------------------------------


# VDSI for Littorina, Buccinum and Nassarius 
#  Littorina littorea
#  Buccinum undatum
#  Nassarius reticulatus
# VDSI for Neptunea and Nucella
#  Neptunea antiqua
#  Nucella lapillus

df.bioeffects.1<-inner_join(df.bioeffects,
                           select(df.thrsh.be,PARAM,Threshold.Species,Threshold.Unit,Threshold.Value),
                           by=c("PARAM"="PARAM","Species"="Threshold.Species"))
df.bioeffects.2<-inner_join(df.bioeffects,
                            select(df.thrsh.be,PARAM,Biota.Type,Threshold.Unit,Threshold.Value),
                            by=c("PARAM"="PARAM","Type"="Biota.Type"))

df.bioeffects<-rbind(df.bioeffects.1,df.bioeffects.2)

df.bioeffects$RESPONSE<-ifelse(df.bioeffects$PARAM=="LMD",-1,1)


# ------------------------ biota -----------------------------------------------------------

cat(paste0(nrow(df.biota)," rows\n"))

# Drop measurements in eggs
df.biota <- filter(df.biota,!MATRX=="EG")

# match Biota with threshold values
df.biota<-inner_join(df.biota,
                    filter(df.thrsh,Category %in% c("Biota")),
                    by=c("PARAM"="PARAM","Type"="Biota.Type"))

cat(paste0(nrow(df.biota)," rows\n"))


df.biota$match<-ifelse(df.biota$MATRX=="LI",
                       ifelse(df.biota$Threshold.Tissue=="MU",0,1),
                       ifelse(df.biota$MATRX=="MU",
                              ifelse(df.biota$Threshold.Tissue=="LI",0,1),
                              1))
cat(paste0(nrow(df.biota)," rows\n"))

df.biota<-df.biota %>%
  filter(match==1) %>%
  select(-c(match))

speciesavg<-df.biota %>%
  group_by(Type,Species,MATRX) %>%
  summarise(DRYWT_SPECIES=mean(DRYWT,na.rm=TRUE),FATWT_SPECIES=mean(FATWT,na.rm=TRUE))
typeavg<-df.biota %>%
  group_by(Type,MATRX) %>%
  summarise(DRYWT_TYPE=mean(DRYWT,na.rm=TRUE),FATWT_TYPE=mean(FATWT,na.rm=TRUE))

speciesavg<-left_join(speciesavg,typeavg)
speciesavg$DRYWT_SPECIES<-ifelse(is.nan(speciesavg$DRYWT_SPECIES),speciesavg$DRYWT_TYPE,speciesavg$DRYWT_SPECIES)
speciesavg$FATWT_SPECIES<-ifelse(is.nan(speciesavg$FATWT_SPECIES),speciesavg$FATWT_TYPE,speciesavg$FATWT_SPECIES)
speciesavg<-select(speciesavg,-c(DRYWT_TYPE,FATWT_TYPE))

df.biota<-left_join(df.biota,speciesavg)
df.biota$DRYWT<-ifelse(is.na(df.biota$DRYWT),df.biota$DRYWT_SPECIES,df.biota$DRYWT)
df.biota$FATWT<-ifelse(is.na(df.biota$FATWT),df.biota$FATWT_SPECIES,df.biota$FATWT)
df.biota<-select(df.biota,-c(DRYWT_SPECIES,FATWT_SPECIES))

cat(paste0(nrow(df.biota)," rows\n"))

df.biota$factor.basis<-ifelse(df.biota$BASIS==df.biota$Threshold.BASIS,1,NA)
df.biota$factor.basis<-ifelse(df.biota$BASIS=="W" & df.biota$Threshold.BASIS=="D",100/df.biota$DRYWT,df.biota$factor.basis)
df.biota$factor.basis<-ifelse(df.biota$BASIS=="W" & df.biota$Threshold.BASIS=="L",100/df.biota$FATWT,df.biota$factor.basis)
df.biota$factor.basis<-ifelse(df.biota$BASIS=="D" & df.biota$Threshold.BASIS=="W",df.biota$DRYWT/100,df.biota$factor.basis)
df.biota$factor.basis<-ifelse(df.biota$BASIS=="D" & df.biota$Threshold.BASIS=="L",df.biota$DRYWT/df.biota$FATWT,df.biota$factor.basis)
df.biota$factor.basis<-ifelse(df.biota$BASIS=="L" & df.biota$Threshold.BASIS=="D",df.biota$FATWT/df.biota$DRYWT,df.biota$factor.basis)
df.biota$factor.basis<-ifelse(df.biota$BASIS=="L" & df.biota$Threshold.BASIS=="W",df.biota$FATWT/100,df.biota$factor.basis)

df.biota<-arrange(df.biota,Country,GridID,MYEAR,DATE,Category,Species,PARAM,GROUP,
                  tblBioID,tblSampleID,tblParamID,
                  Threshold.BASIS,Threshold.Unit,Threshold.Tissue,
                  Value,Multiplier,Threshold.Value,Required)

#find the factor for unit conversion  
df.biota <- left_join(df.biota,df.unit.factor,by=c("MUNIT","Threshold.Unit"))

test<-filter(df.biota,is.na(factor.basis))
test2<-distinct(test,Species,PARAM)

df.biota$Value<-df.biota$Value*df.biota$factor.basis*df.biota$Factor.Unit
df.biota$Unit<-df.biota$Threshold.Unit
df.biota$BASIS<-df.biota$Threshold.BASIS
df.biota$Unit<-df.biota$Threshold.Unit

df.biota<-df.biota %>%
  select(Country,GridID,MYEAR,DATE,Category,Species,PARAM,GROUP,
         BASIS=Threshold.BASIS,UNIT=Threshold.Unit,Tissue=Threshold.Tissue,
         Value,Multiplier,Threshold=Threshold.Value,Required,
         tblBioID,tblSampleID,tblParamID)

df.biota.1<-df.biota %>%
  group_by(Country,GridID,MYEAR,DATE,Category,Species,PARAM,GROUP,
         BASIS,UNIT,Tissue,Multiplier,Threshold,Required) %>%
  summarise(Value=mean(Value,na.rm=TRUE)) %>% 
  ungroup()

df.biota.1$Value<-df.biota.1$Value*df.biota.1$Multiplier

df.biota.2<-df.biota.1 %>%
  group_by(Country,GridID,MYEAR,DATE,Category,Species,GROUP,
           BASIS,UNIT,Tissue,Threshold) %>%
  summarise(Value=sum(Value,na.rm=TRUE),nreq=sum(Required,na.rm=TRUE),n=n())

df.biota.2$CR<-df.biota.2$Value/df.biota.2$Threshold

test<-df.biota.2 %>%
  group_by(Country,GridID,MYEAR,DATE,Category,Species,GROUP) %>%
  summarise(n=n())

# test<-df.biota.norm %>%
#   spread(PARAM,Value)
# 
# df.biota$Category <- "Biota"
# df.sediment$Category <- "Sediment"
# df.water$Category <- "Water"


#--------------------------------------------------------------------------------------------------------------------------------
