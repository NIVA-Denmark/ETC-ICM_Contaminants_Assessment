rm(list=ls())
library("dplyr")
library("tidyr")

cat("Starting assessment.R\n")
#--------------------------------------------------------------------------------------------------------------------------------
file.r.biota <- "data_ICES/data_biota_DOME.Rda"
file.r.sediment <- "data_ICES/data_sediment_DOME.Rda"
file.r.water <- "data_ICES/data_water_DOME.Rda"

file.r.biota.2 <- "data_ICES/data_biota_EIONET.Rda"
file.r.sediment.2 <- "data_ICES/data_sediment_EIONET.Rda"
file.r.water.2 <- "data_ICES/data_water_EIONET.Rda"

file.r.biota.3 <- "data_PT/data_biota_PT.Rda"
file.r.sediment.3 <- "data_PT/data_sediment_PT.Rda"
file.r.water.3 <- "data_PT/data_water_PT.Rda"

datafolder <- "data_ICES/"
thrshfolder <- "thresholds/"
resfolder <- "../gis/CHASE_results/"

df.biota <- readRDS(file.r.biota)
df.sediment <- readRDS(file.r.sediment)
df.water <- readRDS(file.r.water)

df.biota.2 <- readRDS(file.r.biota.2)
df.sediment.2 <- readRDS(file.r.sediment.2)
df.water.2 <- readRDS(file.r.water.2)

df.biota.3 <- readRDS(file.r.biota.3)
df.sediment.3 <- readRDS(file.r.sediment.3)
df.water.3 <- readRDS(file.r.water.3)

df.conversion<-read.table(paste0(datafolder,"species_avg_lipid_drywt_OSPAR.txt"), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.species<-read.table(paste0(datafolder,"species_type.txt"), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.params<-read.table(paste0(datafolder,"PARAM.txt"), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)

#------------------ process thresholds  -----------------------------------------
df.thrsh <- read.table(paste0(thrshfolder,"thresholds.txt"), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.groups <- read.table(paste0(thrshfolder,"groups.txt"), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)
df.unit.factor <- read.table(paste0(thrshfolder,"unit_factor.txt"), quote="",sep="\t", header=TRUE, fileEncoding="UTF-16", stringsAsFactors=FALSE)


df.groups <- select(df.groups,PARAM,GROUP,Multiplier,Required)

df.thrsh <- filter(df.thrsh,PARAM!="")
df.thrsh <- filter(df.thrsh,is.na(Exclude))
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

df.thrsh.sed<-filter(df.thrsh,Category=="Sediment")
df.thrsh.wat<-filter(df.thrsh,Category=="Water")


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
df.biota$MATRX<-ifelse(df.biota$MATRX=="MU&FA","MU",df.biota$MATRX)
df.biota$Species<-ifelse(df.biota$Species=="Clupea harengus membras","Clupea harengus",df.biota$Species)
df.biota$Value<-df.biota$Value*ifelse(df.biota$PARAM %in% c("DRYWT%","EXLIP%","FATWT%","LIPIDWT%") & df.biota$Value > 1000,0.01,1)


df.biota <- left_join(df.biota,df.species,by=c("Species"="Species"))
df.biota <- filter(df.biota,Type %in% c("Fish","Shellfish"))

df.biota$Value <- df.biota$Value*ifelse(df.biota$QFLAG=="<",0.5,1)
df.biota <- select(df.biota,-c(MPROG,RLABO,ALABO,VFLAG,DETLI,METCU,STATN,
                               Latitude,Longitude,NOINP,UNCRT,tblSpotID,
                               tblUploadID,tblAnalysisID,QFLAG,
                               LMQNT))
#EIONET data
# replace species names
df.biota.2$Species<-ifelse(df.biota.2$Species=="","",df.biota.2$Species)
df.biota.2$Species<-ifelse(df.biota.2$Species=="Mitylus galloprovincialis","Mytilus galloprovincialis",df.biota.2$Species)
df.biota.2$Species<-ifelse(df.biota.2$Species=="PERCA FLUVIATILIS","Perca fluviatilis",df.biota.2$Species)
df.biota.2$Species<-ifelse(df.biota.2$Species=="CLUPEA HARENGUS","Clupea harengus",df.biota.2$Species)
df.biota.2$Species<-ifelse(df.biota.2$Species=="Arca noae; Holothuria spp.","Holothuria spp.",df.biota.2$Species)
df.biota.2$Species<-ifelse(df.biota.2$Species=="Holothuria tubulosa","Holothuria spp.",df.biota.2$Species)

df.biota.2 <- left_join(df.biota.2,df.species,by=c("Species"="Species"))
df.biota.2 <- filter(df.biota.2,Type %in% c("Fish","Shellfish"))

df.biota.2$Value <- df.biota.2$Value*ifelse(df.biota.2$QFLAG=="<",0.5,1)
df.biota.2 <- select(df.biota.2,-c(MPROG,RLABO,ALABO,VFLAG,DETLI,METCU,STATN,Determinand_HazSubs,
                               Latitude,Longitude,NOINP,UNCRT,QFLAG,LMQNT,QA_comment,WaterbaseID))




#------------------------- Calculate normalization values: wet weight and lipid weight ---------------------

df.biota.norm <- df.biota %>%
  filter(PARAM %in% c("LNMEA","DRYWT%","EXLIP%","FATWT%","LIPIDWT%")) %>%
  spread(PARAM,Value)

if((length(names(df.biota.norm)[names(df.biota.norm)=="FATWT%"]))<1){
  df.biota.norm[,'FATWT'] <- NA
}else{
  names(df.biota.norm)[names(df.biota.norm)=="FATWT%"]<-"FATWT"
}

if((length(names(df.biota.norm)[names(df.biota.norm)=="EXLIP%"]))<1){
  df.biota.norm[,'EXLIP'] <- NA
}else{
  names(df.biota.norm)[names(df.biota.norm)=="EXLIP%"]<-"EXLIP"
}

if((length(names(df.biota.norm)[names(df.biota.norm)=="LIPIDWT%"]))<1){
  df.biota.norm[,'LIPIDWT'] <- NA
}else{
  names(df.biota.norm)[names(df.biota.norm)=="LIPIDWT%"]<-"LIPIDWT"
}

names(df.biota.norm)[names(df.biota.norm)=="DRYWT%"]<-"DRYWT"

df.biota.norm$FATWT <- ifelse(is.na(df.biota.norm$FATWT),
                              ifelse(is.na(df.biota.norm$LIPIDWT),
                                     df.biota.norm$EXLIP,
                                     df.biota.norm$LIPIDWT),
                              df.biota.norm$FATWT)

df.biota.norm <- df.biota.norm %>%
  select(-c(EXLIP,LIPIDWT))

# remove erroneous values
df.biota.norm$OK <- 1
df.biota.norm$OK <- ifelse(df.biota.norm$DRYWT > 40 & df.biota.norm$Type == "Shellfish",0,df.biota.norm$OK)
df.biota.norm$OK <- ifelse(df.biota.norm$DRYWT < 3 & !is.na(df.biota.norm$DRYWT),0,df.biota.norm$OK)
df.biota.norm$OK <- ifelse(df.biota.norm$DRYWT > 30 & df.biota.norm$Type == "Fish" & df.biota.norm$MATRX == "MU",0,df.biota.norm$OK)


df.biota.norm <- df.biota.norm %>%
  filter(OK == 1)


df.biota.norm.species <- df.biota.norm %>%
  group_by(Type,Species,MATRX,MUNIT) %>%
  summarize(DRYWT_SPECIES=mean(DRYWT,na.rm=TRUE),FATWT_SPECIES=mean(FATWT,na.rm=TRUE)) %>%
  ungroup()

df.biota.norm.type <- df.biota.norm %>%
  group_by(Type,MATRX,MUNIT) %>%
  summarize(DRYWT_TYPE=mean(DRYWT,na.rm=TRUE),FATWT_TYPE=mean(FATWT,na.rm=TRUE)) %>%
  ungroup()

df.biota.norm.species <- df.biota.norm.species %>%
  left_join(df.biota.norm.type,by=c("Type"="Type","MATRX"="MATRX","MUNIT"="MUNIT"))

df.biota.norm.species$DRYWT_SPECIES<-ifelse(is.nan(df.biota.norm.species$DRYWT_SPECIES),
                                            df.biota.norm.species$DRYWT_TYPE,
                                            df.biota.norm.species$DRYWT_SPECIES)
df.biota.norm.species$FATWT_SPECIES<-ifelse(is.nan(df.biota.norm.species$FATWT_SPECIES),
                                            df.biota.norm.species$FATWT_TYPE,
                                            df.biota.norm.species$FATWT_SPECIES)
df.biota.norm.species<-select(df.biota.norm.species,-c(DRYWT_TYPE,FATWT_TYPE))


df.biota.norm <- df.biota.norm %>%
  group_by(tblBioID,tblSampleID,Type,Species,MATRX,MUNIT) %>%
  summarize(DRYWT=mean(DRYWT,na.rm=TRUE),FATWT=mean(FATWT,na.rm=TRUE)) %>%
  ungroup()

df.biota.norm <- df.biota.norm %>%
  left_join(df.biota.norm.species,by=c("Type"="Type","Species"="Species","MATRX"="MATRX","MUNIT"="MUNIT"))

df.biota.norm$DRYWT<-ifelse(is.nan(df.biota.norm$DRYWT),
                            ifelse(is.nan(df.biota.norm$DRYWT_SPECIES),NA,df.biota.norm$DRYWT_SPECIES),
                            df.biota.norm$DRYWT)
df.biota.norm$FATWT<-ifelse(is.nan(df.biota.norm$FATWT),
                            ifelse(is.nan(df.biota.norm$FATWT_SPECIES),NA,df.biota.norm$FATWT_SPECIES),
                            df.biota.norm$FATWT)
df.biota.norm<-select(df.biota.norm,-c(DRYWT_SPECIES,FATWT_SPECIES))



# Add the normalization values back to the main biota data
df.biota <- filter(df.biota,!PARAM %in% c("LNMEA","DRYWT%","EXLIP%","FATWT%","LIPIDWT%"))
df.biota <-left_join(df.biota, df.biota.norm)

# -------------- add the EIONET data here --------------------

names(df.biota.2)[names(df.biota.2)=="DRYWT."]<-"DRYWT"
names(df.biota.2)[names(df.biota.2)=="EXLIP."]<-"EXLIP"
names(df.biota.2)[names(df.biota.2)=="FATWT."]<-"FATWT"
names(df.biota.2)[names(df.biota.2)=="LIPIDWT."]<-"LIPIDWT"

df.biota.2$FATWT <- ifelse(is.na(df.biota.2$FATWT),
                              ifelse(is.na(df.biota.2$LIPIDWT),
                                     df.biota.2$EXLIP,
                                     df.biota.2$LIPIDWT),
                              df.biota.2$FATWT)

df.biota.2<-df.biota.2%>% select(-c(EXLIP,LIPIDWT))

df.biota<-bind_rows(df.biota,df.biota.2)

# ------------------------------------------------------------------------------------------
# ------------------------ bioeffects ------------------------------------------------------
# ------------------------------------------------------------------------------------------

df.thrsh.be <- filter(df.thrsh,Category=="BioEffects") #Threshold.Species,
df.thrsh <- select(df.thrsh,PARAM,GROUP,Category,Biota.Type,Threshold.Unit,Threshold.BASIS,Threshold.Tissue,Threshold.Value,Multiplier,Required)

rm(list=c("df.biota.DRYWT","df.biota.FATWT","df.biota.EXLIP","df.biota.LIPIDWT"))
#rm(list=c("df.biota.DRYWT","df.biota.FATWT","df.biota.EXLIP","df.biota.LIPIDWT","df.biota.norm"))

df.bioeffects<-filter(df.biota,PARAM %in% c("VDSI","MNC","LMD"))
df.biota<-filter(df.biota,!PARAM %in% c("VDSI","MNC","LMD"))



# VDSI for Littorina, Buccinum and Nassarius 
#  Littorina littorea
#  Buccinum undatum
#  Nassarius reticulatus
# VDSI for Neptunea and Nucella
#  Neptunea antiqua
#  Nucella lapillus

df.bioeffects.1<-inner_join(df.bioeffects,
                            select(df.thrsh.be,Substance.name,PARAM,Threshold.Species,Threshold.Unit,Threshold.Value),
                            by=c("PARAM"="PARAM","Species"="Threshold.Species"))
df.bioeffects.2<-inner_join(df.bioeffects,
                            select(df.thrsh.be,Substance.name,PARAM,Biota.Type,Threshold.Unit,Threshold.Value),
                            by=c("PARAM"="PARAM","Type"="Biota.Type"))

df.bioeffects<-rbind(df.bioeffects.1,df.bioeffects.2)
rm(list=c("df.bioeffects.1","df.bioeffects.2"))

df.bioeffects$RESPONSE<-ifelse(df.bioeffects$PARAM=="LMD",-1,1)

df.bioeffects<-df.bioeffects %>%
  group_by(Country,GridID,MYEAR,DATE,Substance.name,PARAM,
           Threshold.Unit,Threshold.Value,RESPONSE) %>%
  summarise(Value=sum(Value,na.rm=TRUE),n=n()) %>%
  ungroup()

#df.bioeffects.1<-filter(df.bioeffects.1,GridID=="100kmE40N37")
# We now have station/date averages (and sums for ggroup variables) 

# Take the average (+median within GridID)
df.bioeffects<-df.bioeffects %>%
  group_by(GridID,Substance.name,PARAM,Threshold.Unit,Threshold.Value,RESPONSE) %>%
  summarise(Median=median(Value,na.rm=TRUE),Value=mean(Value,na.rm=TRUE),n=n()) %>%
  ungroup()

df.bioeffects$CR<-ifelse(df.bioeffects$RESPONSE<1,df.bioeffects$Threshold.Value/df.bioeffects$Value,df.bioeffects$Value/df.bioeffects$Threshold.Value)

df.chase.bioeffects<-df.bioeffects %>%
  group_by(GridID) %>%
  summarise(SumCR=sum(CR,na.rm=TRUE),n=n()) %>%
  ungroup()

df.chase.bioeffects$CSum<-df.chase.bioeffects$SumCR/df.chase.bioeffects$n


# ------------------------------------------------------------------------------------------
# ------------------------ biota -----------------------------------------------------------
# ------------------------------------------------------------------------------------------

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

df.biota$Value<-df.biota$Value*df.biota$factor.basis*df.biota$Factor.Unit
df.biota$BASIS<-df.biota$Threshold.BASIS
df.biota$Unit<-df.biota$Threshold.Unit

df.biota<-df.biota %>%
  select(Country,GridID,MYEAR,DATE,Category,Type,Species,PARAM,GROUP,
         BASIS=Threshold.BASIS,UNIT=Threshold.Unit,Tissue=Threshold.Tissue,
         Value,Multiplier,Threshold=Threshold.Value,Required,
         tblBioID,tblSampleID,tblParamID)

df.biota<-df.biota %>%
  group_by(Country,GridID,MYEAR,DATE,Category,Type,Species,PARAM,GROUP,
           BASIS,UNIT,Tissue,Multiplier,Threshold,Required) %>%
  summarise(Value=mean(Value,na.rm=TRUE)) %>% 
  ungroup()

df.biota$Value<-df.biota$Value*df.biota$Multiplier


df.biota<-df.biota %>%
  group_by(Country,GridID,MYEAR,DATE,Category,Type,Species,GROUP,
           BASIS,UNIT,Tissue,Threshold) %>%
  summarise(Value=sum(Value,na.rm=TRUE),nreq=sum(Required,na.rm=TRUE),n=n()) %>%
  ungroup()

# We now have station/date averages (and sums for ggroup variables) 

# Take the average (+median within GridID)
df.biota<-df.biota %>%
  group_by(GridID,Category,Type,GROUP,BASIS,UNIT,Tissue,Threshold) %>%
  summarise(Value=mean(Median=median(Value,na.rm=TRUE),Value,na.rm=TRUE),n=n()) %>%
  ungroup()

df.biota$CR<-df.biota$Value/df.biota$Threshold

df.chase.biota<-df.biota %>%
  group_by(GridID) %>%
  summarise(SumCR=sum(CR,na.rm=TRUE),n=n()) %>%
  ungroup()

df.chase.biota$CSum<-df.chase.biota$SumCR/sqrt(df.chase.biota$n)

# ------------------------------------------------------------------------------------------
# ------------------------ sediment ------------------------------------------------------
# ------------------------------------------------------------------------------------------
df.sed <- df.sediment
df.sed$MUNIT <- ifelse(df.sed$MUNIT=="ug Sn/kg","ug/kg",df.sed$MUNIT)
df.sed$Value <- df.sed$Value*ifelse(df.sed$QFLAG=="<",0.5,1)
df.sed <- select(df.sed,-c(MPROG,RLABO,ALABO,VFLAG,DETLI,METCU,STATN,
                           Latitude,Longitude,UNCRT,tblSpotID,
                           tblUploadID,tblAnalysisID,QFLAG,
                           LMQNT))
df.sed <- filter(df.sed,DEPHU<0.01)
df.sed$PARAM<-ifelse(df.sed$PARAM=="Cr","CR",df.sed$PARAM)


# find normalization parameters (C-org, Al)
df.sed.norm <- df.sed %>%
  filter(PARAM %in% c("AL","DRYWT%","Li","CORG")) %>%
  group_by(tblSampleID,PARAM,MATRX,BASIS,MUNIT) %>%
  summarize(Value=mean(Value,na.rm=TRUE)) %>%
  ungroup()

unit<-c("g/kg","mg/kg","ug/kg","ug/g","%")
factor<-c(0.001,1e-6,1e-9,1e-6,1)
factors<-data.frame(unit,factor,stringsAsFactors = FALSE)

df.sed.DRYWT <- df.sed.norm %>%
  filter(PARAM == "DRYWT%") %>%
  select(tblSampleID,MATRX,BASIS,Value,MUNIT) %>%
  left_join(factors,by=c("MUNIT"="unit")) %>%
  mutate(DRYWT=Value*factor,MUNIT="%") %>%
  select(-c(factor,MUNIT,Value))

df.sed.AL <- df.sed.norm %>%
  filter(PARAM == "AL") %>%
  select(tblSampleID,MATRX,BASIS,Value,MUNIT) %>%
  left_join(factors,by=c("MUNIT"="unit"))  %>%
  mutate(AL=Value*factor,MUNIT="%") %>%
  select(-c(factor,MUNIT,Value))

df.sed.CORG <- df.sed.norm %>%
  filter(PARAM == "CORG") %>%
  select(tblSampleID,MATRX,BASIS,Value,MUNIT) %>%
  left_join(factors,by=c("MUNIT"="unit")) %>%
  mutate(CORG=Value*factor,MUNIT="%") %>%
  select(-c(factor,MUNIT,Value))

df.sed.norm<-full_join(df.sed.DRYWT,df.sed.AL,by=c("tblSampleID","MATRX","BASIS"))
df.sed.norm<-full_join(df.sed.norm,df.sed.CORG,by=c("tblSampleID","MATRX","BASIS"))

rm(list=c("df.sed.AL","df.sed.CORG","df.sed.DRYWT"))
df.sed <- df.sed %>%
  filter(!PARAM %in% c("AL","DRYWT%","Li","CORG")) 

# Add normalization parameters back to data
df.sed <- left_join(df.sed,df.sed.norm,by=c("tblSampleID","MATRX","BASIS"))

# Add the EIONET data

df.sed.2 <- df.sediment.2
df.sed.2$BASIS <- ifelse(df.sed.2$BASIS=="L","D",df.sed.2$BASIS)
df.sed.2$Value <- df.sed.2$Value*ifelse(df.sed.2$QFLAG=="<",0.5,1)
df.sed.2 <- select(df.sed.2,-c(MPROG,RLABO,ALABO,VFLAG,DETLI,METCU,STATN,Determinand_HazSubs,
                           Latitude,Longitude,UNCRT,QFLAG,LMQNT,
                           QA_comment,WaterbaseID,Sampler,BottomDepth))

names(df.sed.2)[names(df.sed.2)=="DRYWT."]<-"DRYWT"

df.sed.2 <- filter(df.sed.2,DEPHU<0.01)
df.sed.2$PARAM<-ifelse(df.sed.2$PARAM=="Cr","CR",df.sed.2$PARAM)


df.sed <- bind_rows(df.sed,df.sed.2)

# Correction of Sediment CORG, AL
df.sed$CORG<-df.sed$CORG*ifelse(df.sed$CORG < 0.1,100,1)
df.sed$AL<-df.sed$AL*ifelse(df.sed$AL<0.1,100,1)
df.sed$AL<-ifelse(df.sed$AL<0.1,NA,df.sed$AL)

# calculate global average for CORG, DRYWT, AL
select<-df.sed %>% distinct(tblSampleID,GridID,DATE,CORG) %>% filter(CORG>0)
select$lnCORG<-log(select$CORG)
corgavg<-exp(mean(log(select$CORG),na.rm=T))
ggplot(select,aes(lnCORG)) + theme_grey(base_size = 5)+geom_histogram(binwidth=0.1)

select<-df.sed %>% distinct(tblSampleID,GridID,DATE,DRYWT) %>% filter(DRYWT<90,DRYWT>1)
drywtavg<-exp(mean(log(select$DRYWT),na.rm=T))
ggplot(select,aes(DRYWT)) + theme_grey(base_size = 5)+geom_histogram(binwidth=1)

select<-df.sed %>% distinct(tblSampleID,GridID,DATE,AL) %>% filter(AL<20,AL>0.1)
alavg<-mean(select$AL,na.rm=T)
ggplot(select,aes(AL)) + theme_grey(base_size = 5)+geom_histogram(binwidth=0.1)


# use the relationship between ln(WETWT) and ln(CORG): ln(WETWT)=3.68+0.323*ln(CORG)
df.sed$DRYWT<-ifelse(is.na(df.sed$DRYWT),
                     ifelse(df.sed$CORG<15,100-39.6*(df.sed$CORG^0.323),NA),
                     df.sed$DRYWT)

df.sed$CORG<-ifelse(is.na(df.sed$CORG),
                    ifelse(df.sed$DRYWT>5,1.127e-05*((100-df.sed$DRYWT)^3.096),NA),
                    df.sed$CORG)

# If still missing, then use global averages
df.sed$AL<-ifelse(is.na(df.sed$AL),alavg,df.sed$AL)
df.sed$CORG<-ifelse(is.na(df.sed$CORG),corgavg,df.sed$CORG)
df.sed$DRYWT<-ifelse(is.na(df.sed$DRYWT),drywtvg,df.sed$DRYWT)



# Add threshold values
df.thrsh.sed <- df.thrsh.sed %>%
  select(PARAM,GROUP,Threshold.Unit,Threshold.BASIS,REF.PARAM,REF.VALUE,REF.UNIT,Threshold.Value,Threshold.Unit,Required)
df.thrsh.sed$REF.PARAM<-ifelse(df.thrsh.sed$REF.PARAM=="","CORG",df.thrsh.sed$REF.PARAM)
df.thrsh.sed$REF.VALUE<-ifelse(is.na(df.thrsh.sed$REF.VALUE),
                               ifelse(df.thrsh.sed$REF.PARAM=="CORG",2.5,
                                      ifelse(df.thrsh.sed$REF.PARAM=="AL",5,
                                             df.thrsh.sed$REF.VALUE)),
                               df.thrsh.sed$REF.VALUE)
df.thrsh.sed$REF.UNIT<-"%"

df.sed<-left_join(df.sed,df.thrsh.sed,by=c("PARAM"="PARAM"))

df.sed$factor<-ifelse(df.sed$REF.PARAM=="AL" & !is.na(df.sed$AL) & df.sed$AL > 0.5,df.sed$AL*df.sed$REF.VALUE,1)
df.sed$factor<-ifelse(df.sed$REF.PARAM=="CORG" & !is.na(df.sed$CORG),df.sed$CORG*df.sed$REF.VALUE,df.sed$factor)

df.sed <- left_join(df.sed,df.unit.factor,by=c("MUNIT","Threshold.Unit"))

df.sed$Value<-df.sed$Value*df.sed$factor*df.sed$Factor.Unit

df.sed$BASIS<-df.sed$Threshold.BASIS
df.sed$Unit<-df.sed$Threshold.Unit

df.sed<-df.sed %>%
  filter(!is.na(Threshold.Value)) %>%
  select(Country,GridID,MYEAR,DATE,PARAM,GROUP,Unit,Value,Threshold=Threshold.Value,Required) %>%
  arrange(Country,GridID,MYEAR,DATE,PARAM,GROUP,Unit,Threshold)

df.sed<-df.sed %>%
  group_by(Country,GridID,MYEAR,DATE,PARAM,GROUP,Unit,Threshold,Required) %>%
  summarise(Value=mean(Value),na.rm=TRUE) %>%
  ungroup()

# combine group variables
df.sed<-df.sed %>%
  group_by(Country,GridID,MYEAR,DATE,GROUP,Unit,Threshold,Required) %>%
  summarise(Value=sum(Value),n=n(),nreq=sum(Required)) %>%
  ungroup()

df.sed$CR<-df.sed$Value/df.sed$Threshold

df.chase.sed<-df.sed %>%
  group_by(GridID) %>%
  summarise(SumCR=sum(CR,na.rm=TRUE),n=n()) %>%
  ungroup()

df.chase.sed$CSum<-df.chase.sed$SumCR/sqrt(df.chase.sed$n)

# ------------------------------------------------------------------------------------------
# ------------------------ sediment ------------------------------------------------------
# ------------------------------------------------------------------------------------------


df.wat <- df.water
df.wat$MUNIT <- ifelse(df.wat$MUNIT=="ug Sn/kg","ug/kg",df.wat$MUNIT)
df.wat$Value <- df.wat$Value*ifelse(df.wat$QFLAG=="<",0.5,1)
df.wat <- df.wat %>%
  select(Country,GridID,MYEAR,DATE,PARAM,Value,MUNIT) %>%
  left_join(select(df.thrsh.wat,GROUP,PARAM,Threshold.Unit,Threshold.Value,Required),by=c("PARAM")) %>%
  filter(!is.na(Threshold.Value))

df.wat<-df.wat %>%
  left_join(df.unit.factor,by=c("MUNIT","Threshold.Unit"))

df.wat$Value<-df.wat$Value*df.wat$Factor.Unit
df.wat <- df.wat %>%
  select(Country,GridID,MYEAR,DATE,GROUP,PARAM,Unit=Threshold.Unit,Value,Threshold=Threshold.Value,Required)

# combine group variables
df.wat<-df.wat %>%
  group_by(Country,GridID,MYEAR,DATE,GROUP,Unit,Threshold,Required) %>%
  summarise(Value=sum(Value),n=n(),nreq=sum(Required)) %>%
  ungroup()

df.wat$CR<-df.wat$Value/df.wat$Threshold

df.chase.wat<-df.wat %>%
  group_by(GridID) %>%
  summarise(SumCR=sum(CR,na.rm=TRUE),n=n()) %>%
  ungroup()

df.chase.wat$CSum<-df.chase.wat$SumCR/sqrt(df.chase.wat$n)

# ------------------------------------------------------------------------------------------
# ------------------------ SUMMARY ------------------------------------------------------
# ------------------------------------------------------------------------------------------

df.chase.sed$Category<-"Sediment"
df.chase.biota$Category<-"Biota"
df.chase.bioeffects$Category<-"Bio.Effects"
df.chase.wat$Category<-"Water"

df.CHASE.QE<-rbind(df.chase.bioeffects,df.chase.biota,df.chase.sed,df.chase.wat)

df.CHASE.QE <- df.CHASE.QE %>%
  filter(GridID!="")

df.CHASE <- df.CHASE.QE %>%
  group_by(GridID) %>%
  summarise(CHASE=max(CSum))

df.CHASE <- df.CHASE %>%
  left_join(select(df.CHASE.QE,GridID,CSum,Category),by=c("GridID"="GridID","CHASE"="CSum")) %>%
  mutate(Worst=Category) %>%
  select(-c(Category))

df.CHASE_TRNSP<-df.CHASE.QE %>%
  select(GridID,Category,CSum) %>%
  ungroup() %>%
  spread(Category,CSum) %>%
  left_join(df.CHASE,by=c("GridID"="GridID"))

df.CHASE_TRNSP$CHASE_CAT<-ifelse(df.CHASE_TRNSP$CHASE>0.5,2,1)
df.CHASE_TRNSP$CHASE_CAT<-ifelse(df.CHASE_TRNSP$CHASE>1,3,df.CHASE_TRNSP$CHASE_CAT)
df.CHASE_TRNSP$CHASE_CAT<-ifelse(df.CHASE_TRNSP$CHASE>5,4,df.CHASE_TRNSP$CHASE_CAT)
df.CHASE_TRNSP$CHASE_CAT<-ifelse(df.CHASE_TRNSP$CHASE>10,5,df.CHASE_TRNSP$CHASE_CAT)

df.CHASE_TRNSP$BIOEFF_CAT<-ifelse(df.CHASE_TRNSP$Bio.Effects>0.5,2,1)
df.CHASE_TRNSP$BIOEFF_CAT<-ifelse(df.CHASE_TRNSP$Bio.Effects>1,3,df.CHASE_TRNSP$BIOEFF_CAT)
df.CHASE_TRNSP$BIOEFF_CAT<-ifelse(df.CHASE_TRNSP$Bio.Effects>5,4,df.CHASE_TRNSP$BIOEFF_CAT)
df.CHASE_TRNSP$BIOEFF_CAT<-ifelse(df.CHASE_TRNSP$Bio.Effects>10,5,df.CHASE_TRNSP$BIOEFF_CAT)

df.CHASE_TRNSP$BIOTA_CAT<-ifelse(df.CHASE_TRNSP$Biota>0.5,2,1)
df.CHASE_TRNSP$BIOTA_CAT<-ifelse(df.CHASE_TRNSP$Biota>1,3,df.CHASE_TRNSP$BIOTA_CAT)
df.CHASE_TRNSP$BIOTA_CAT<-ifelse(df.CHASE_TRNSP$Biota>5,4,df.CHASE_TRNSP$BIOTA_CAT)
df.CHASE_TRNSP$BIOTA_CAT<-ifelse(df.CHASE_TRNSP$Biota>10,5,df.CHASE_TRNSP$BIOTA_CAT)

df.CHASE_TRNSP$SED_CAT<-ifelse(df.CHASE_TRNSP$Sediment>0.5,2,1)
df.CHASE_TRNSP$SED_CAT<-ifelse(df.CHASE_TRNSP$Sediment>1,3,df.CHASE_TRNSP$SED_CAT)
df.CHASE_TRNSP$SED_CAT<-ifelse(df.CHASE_TRNSP$Sediment>5,4,df.CHASE_TRNSP$SED_CAT)
df.CHASE_TRNSP$SED_CAT<-ifelse(df.CHASE_TRNSP$Sediment>10,5,df.CHASE_TRNSP$SED_CAT)

df.CHASE_TRNSP$WAT_CAT<-ifelse(df.CHASE_TRNSP$Water>0.5,2,1)
df.CHASE_TRNSP$WAT_CAT<-ifelse(df.CHASE_TRNSP$Water>1,3,df.CHASE_TRNSP$WAT_CAT)
df.CHASE_TRNSP$WAT_CAT<-ifelse(df.CHASE_TRNSP$Water>5,4,df.CHASE_TRNSP$WAT_CAT)
df.CHASE_TRNSP$WAT_CAT<-ifelse(df.CHASE_TRNSP$Water>10,5,df.CHASE_TRNSP$WAT_CAT)


#write.table(df.CHASE_TRNSP,file=paste0(resfolder,"CHASE_results.csv"), row.names=FALSE,quote=FALSE,sep=',', na="")
