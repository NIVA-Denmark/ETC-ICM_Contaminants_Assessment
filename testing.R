
#-----------------------------------------------------------------------
test <- df.biota %>%
  filter(PARAM %in% c("LNMEA","DRYWT%","EXLIP%","FATWT%","LIPIDWT%"))

test1 <- df.biota.in %>%
  filter(PARAM %in% c("DRYWT%") & MATRX != "LI" 
         & Species %in% c("Mytilus edulis","Mytilus galloprovincialis")
         & Value < 1)  %>%
  arrange(Value)


test2 <- df.biota.in %>%
  filter(PARAM %in% c("DRYWT%") & MATRX != "LI" 
         & Species %in% c("Mytilus galloprovincialis")
         & Value >79)  %>%
  arrange(Value)

test <- df.biota.in %>%
  filter(PARAM %in% c("DRYWT%","EXLIP%","FATWT%","LIPIDWT%") & MATRX != "LI" & Value > 1000)  %>%
  arrange(desc(Value))
write.table(test,file=paste0(datafolder,"DOME_errors.txt"), row.names=FALSE,quote=FALSE,sep='\t', na="")

test <- df.biota.in %>%
  filter(PARAM %in% c("DRYWT%") & Species %in% c("Mytilus edulis","Mytilus galloprovincialis") & Value > 70)  %>%
  arrange(desc(Value))

test<-rbind(test,test1,test2)
write.table(test,file=paste0(datafolder,"DOME_errors.txt"), row.names=FALSE,quote=FALSE,sep='\t', na="")

test2 <- filter(df.biota.in,MUNIT=="umol/min/mg protein")


# -------------- plot ---------------------------------------------------
library(ggplot2)
speciesavg<-df.biota.norm %>%
  group_by(Type,Species,MATRX) %>%
  summarise(DRYWT_SPECIES=mean(DRYWT,na.rm=TRUE),FATWT_SPECIES=mean(FATWT,na.rm=TRUE))
typeavg<-df.biota %>%
  group_by(Type,MATRX) %>%
  summarise(DRYWT_TYPE=mean(DRYWT,na.rm=TRUE),FATWT_TYPE=mean(FATWT,na.rm=TRUE))

speciesavg$Tissue<-ifelse(speciesavg$Type=="Fish",
                          ifelse(speciesavg$MATRX=="LI","Fish Liver","Fish Muscle"),
                          "Shellfish")
speciesavg$lnDrywt<-log(speciesavg$DRYWT_SPECIES)
speciesavg$lnFatwt<-log(speciesavg$FATWT_SPECIES)

dfsort<-arrange(speciesavg,Tissue,desc(DRYWT_SPECIES))
speciessort<-distinct

speciesavg$Species<-ifelse(speciesavg$Tissue=="Fish Muscle" & speciesavg$lnDrywt<3.1,"",speciesavg$Species)


p<-ggplot(speciesavg,aes(x=lnDrywt,y=lnFatwt))
p<-p+facet_wrap(~ Tissue,scales="free", ncol=2)
p<-p+geom_text(aes(label = Species))
p<-p+geom_jitter(size=2)
p<-p+geom_smooth(method='lm',formula=y~x,na.rm=T)
#p<-p+scale_x_log10()+scale_y_log10()
ggsave("lin_regr_drywt_fatwt.png", plot=p,
      width=30, height=10, units="cm",
      dpi=300)

df<-df.biota.norm %>%
  select(Species,Type,MATRX,DRYWT,FATWT)
df$lnDrywt<-log(df$DRYWT)
df$lnFatwt<-log(df$FATWT)
df$Tissue<-ifelse(df$Type=="Fish",
                          ifelse(df$MATRX=="LI","Fish Liver","Fish Muscle"),
                          "Shellfish")

p<-ggplot(df,aes(x=Species,y=lnFatwt))
p<-p+facet_wrap(~Tissue,scales="free", ncol=2)
p <- p+geom_boxplot() +theme(axis.text.x=element_text(angle=90,hjust=1))
ggsave("boxplot_lnfatwt.png", plot=p,width=30, height=30, units="cm",dpi=300)

p<-ggplot(df,aes(x=Species,y=lnDrywt))
p<-p+facet_wrap(~Tissue,scales="free", ncol=2)
p <- p+geom_boxplot() +theme(axis.text.x=element_text(angle=90,hjust=1))
ggsave("boxplot_lndrywt.png", plot=p,width=30, height=30, units="cm",dpi=300)


p<-ggplot(df,aes(x=Species,y=FATWT))
p<-p+facet_wrap(~Tissue,scales="free", ncol=2)
p <- p+geom_boxplot() +theme(axis.text.x=element_text(angle=90,hjust=1))
ggsave("boxplot_fatwt.png", plot=p,width=30, height=30, units="cm",dpi=300)

p<-ggplot(df,aes(x=Species,y=DRYWT))
p<-p+facet_wrap(~Tissue,scales="free", ncol=2)
p <- p+geom_boxplot() +theme(axis.text.x=element_text(angle=90,hjust=1))
ggsave("boxplot_drywt.png", plot=p,width=30, height=30, units="cm",dpi=300)


# Histogram FATWT - all species & tissues
df2<-df
df2$Sort<-paste0(df2$Type," ",df2$MATRX," ",df2$Species)

p<-ggplot(df2,aes(FATWT)) + theme_grey(base_size = 5) 
p<-p+facet_wrap(~Sort,scales="free", ncol=6)
p<-p+geom_histogram(binwidth=1)

ggsave("histogram_fatwt_all.png", plot=p,width=18, height=27, units="cm",dpi=300)


# Histogram DRYWT - all species & tissues
df2<-df
df2$Sort<-paste0(df2$Type," ",df2$MATRX," ",df2$Species)

p<-ggplot(df2,aes(DRYWT)) + theme_grey(base_size = 5) 
p<-p+facet_wrap(~Sort,scales="free", ncol=6)
p<-p+geom_histogram(binwidth=1)

ggsave("histogram_drywt_all2.png", plot=p,width=18, height=27, units="cm",dpi=300)

# Histogram - selected
df2<-df %>%
  filter(Species %in% c("Mytilus edulis","Mytilus galloprovincialis"))
p<-ggplot(df2,aes(DRYWT))
p<-p+facet_wrap(~Species,scales="free",ncol=2)
p<-p+geom_histogram(binwidth=1)
ggsave("histogram_drywt_mytilus.png", plot=p,width=18, height=9, units="cm",dpi=600)

df2 %>% group_by(Species) %>%
  summarise(`10%`=quantile(DRYWT, probs=0.1, na.rm=T),
            `90%`=quantile(DRYWT, probs=0.90, na.rm=T),
            avg=mean(DRYWT, na.rm=T),
            med=median(DRYWT,na.rm=T),
            n=n())

df2<-df %>%
  filter(Species %in% c("Gadus morhua"))
p<-ggplot(df2,aes(DRYWT))
p<-p+facet_wrap(~MATRX,scales="free",ncol=2)
p<-p+geom_histogram(binwidth=1)
ggsave("histogram_drywt_gadus.png", plot=p,width=18, height=9, units="cm",dpi=600)


# try to sort in order of fatwt and drywt

# -------------- plot ---------------------------------------------------

crcount<-df.water %>%
  filter(PARAM %in% c("CR","Cr")) %>% 
  group_by(PARAM) %>% 
  summarise(n=n())

library("ggplot2")
p <- ggplot(df,aes(Value)) + geom_histogram(binwidth=0.1)




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

df<-rbind(df.biota,df.sediment)

n2$biota<-n2$sediment
n3$biota<-n3$water

n<-full_join(n1,n2)
n<-full_join(n,n3)

test <- df.biota %>%
  group_by_(variable.Assessment.Unit,variable.Station) %>%
  ungroup %>%
  group_by_(variable.Assessment.Unit) %>%
  summarise(n=n())


paramcount<-df.biota %>% 
  group_by(PARAM) %>%
  summarize(count=n())


test<-df.biota %>% 
  group_by(Species,MATRX) %>%
  summarize(count=n())

test <- test %>%
  spread(MATRX,count)


test<-df.biota %>%
  group_by(Species) %>%
  summarise(count=n())
distinct(df.biota,Species)
param<-distinct(df.biota,PARAM)
param<-left_join(param,df.params,by=c("PARAM"="Code"))

write.table(test,file=paste0(datafolder,"test.csv"), row.names=FALSE,quote=FALSE,sep=',')


test1<-distinct(df.biota,PARAM,MUNIT)
test2<-distinct(df.sediment,PARAM,MUNIT)
test3<-distinct(df.water,PARAM,MUNIT)
test<-rbind(test1,test2,test3)

test1<-distinct(df.biota,PARAM)
test2<-distinct(df.sediment,PARAM)
test3<-distinct(df.water,PARAM)
test<-rbind(test1,test2,test3)
test<-test%>%
  distinct(PARAM) %>%
  arrange(PARAM)



test<-filter(df.biota,MATRX=="LI",BASIS=="D",Threshold.BASIS=="W",Threshold.Tissue=="MU")
test2<-filter(df.biota,tblSampleID==1841817,PARAM=="PB")



test<-filter(df.biota,GridID=="20kmE382N316",PARAM=="CB138",MATRX=="MU",tblBioID==3324169)


test<-df.biota %>%
  distinct(Type,MATRX,BASIS,Threshold.BASIS) %>%
  arrange(Type,MATRX,BASIS)

test<-df.biota %>%
  distinct(Type,BASIS,Threshold.BASIS) %>%
  arrange(Type,BASIS)



test2<-df.biota %>%
  distinct(MUNIT,Threshold.Unit) %>%
  arrange(MUNIT,Threshold.Unit)






df.biota<-spread(df.biota,BASIS,Value)


test<-df.biota
test$count<-ifelse(is.na(test$D),0,1)+ifelse(is.na(test$L),0,1)+ifelse(is.na(test$W),0,1)
test<-filter(test,count>1)


cat(paste0(nrow(df.biota)," rows\n"))

test2<-filter(df.biota,tblAnalysisID==115194)


test1<-filter(df.biota.norm,MATRX=="MU",tblBioID==3324169)

test1<-filter(df.biota.DRYWT,GridID=="20kmE382N316",PARAM=="CB138",MATRX=="MU",tblBioID==3324169)





df <- df.biota %>%
  filter(PARAM %in% c("LNMEA","DRYWT%","EXLIP%","FATWT%","LIPIDWT%")) %>%
  group_by(PARAM,MUNIT) %>%
  summarise()

# ---------------------------------------- DRYWT vs CORG ---------------------------------------------

library(ggplot2)
df2<-df.sed
p1<-ggplot(df2,aes(DRYWT)) + theme_grey(base_size = 5) 
p1<-p1+geom_histogram(binwidth=1)

df2<-df.sed.2
p2<-ggplot(df2,aes(DRYWT)) + theme_grey(base_size = 5) 
p2<-p2+geom_histogram(binwidth=1)
p1
p2

df2<-df.sed %>%
  group_by(tblSampleID,DRYWT,CORG) %>%
  summarise()
df2$DRYWT<-ifelse(df2$DRYWT>80,NA,df2$DRYWT)
df2$lnDRYWT<-log(df2$DRYWT)
df2$lnCORG<-log(df2$CORG)
df2$lnDRYWT<-ifelse(df2$lnDRYWT< 2,NA,df2$lnDRYWT)
df2$Check<- 2.5 - (df2$lnCORG / 2.5)
df2$CORR<-ifelse(df2$lnDRYWT < df2$Check, "1", "0")
df2$lnCORG<-df2$lnCORG+ifelse(df2$CORR=="1",log(100),0)
df2$CORG<-exp(df2$lnCORG)
df2$WETWT<- 100-df2$DRYWT
df2$lnWETWT<- log(df2$WETWT)
df2$DRYWTinv<- ifelse(df2$DRYWT>5,1/df2$DRYWT,NA)



df3<-df.sed.2 %>%
  group_by(GridID,DATE,DRYWT,CORG) %>%
  summarise()
df3$lnDRYWT<-log(df3$DRYWT)
df3$lnCORG<-log(df3$CORG)
df3$Check<- 2.5 - (df3$lnCORG / 2.5)
df3$CORR<-"2"
df3$WETWT<- 100-df3$DRYWT
df3$lnWETWT<- log(df3$WETWT)

df4<-bind_rows(df2,df3)

# plot DOME 
fit<-lm(formula=lnWETWT~lnCORG,data=df2)

p<-ggplot(df2,aes(x=lnCORG,y=lnWETWT))
p<-p+geom_jitter(size=2,aes(colour=CORR))
p<-p+geom_smooth(method='lm',formula=y~x,na.rm=T)
p<-p+labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
                        "Intercept =",signif(fit$coef[[1]],3 ),
                        " Slope =",signif(fit$coef[[2]], 3),
                        " P =",signif(summary(fit)$coef[2,4], 3)))
p
ggsave("lin_regr_sed_corg_wetwt.png", plot=p,
       width=20, height=20, units="cm",
       dpi=300)

# plot EIONET
fit<-lm(formula=lnWETWT~lnCORG,data=df3)

p<-ggplot(df3,aes(x=lnCORG,y=lnWETWT))
p<-p+geom_jitter(size=2,aes(colour=CORR))
p<-p+geom_smooth(method='lm',formula=y~x,na.rm=T)
p<-p+labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
                        "Intercept =",signif(fit$coef[[1]],3 ),
                        " Slope =",signif(fit$coef[[2]], 3),
                        " P =",signif(summary(fit)$coef[2,4], 3)))
p
ggsave("lin_regr_sed_corg_wetwt_EIONET.png", plot=p,
       width=20, height=20, units="cm",
       dpi=300)

# plot DOME+EIONET combined
fit<-lm(formula=lnWETWT~lnCORG,data=df4)

p<-ggplot(df4,aes(x=lnCORG,y=lnWETWT))
p<-p+geom_jitter(size=2,aes(colour=CORR))
p<-p+geom_smooth(method='lm',formula=y~x,na.rm=T)
p<-p+labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
                        "Intercept =",signif(fit$coef[[1]],3 ),
                        " Slope =",signif(fit$coef[[2]], 3),
                        " P =",signif(summary(fit)$coef[2,4], 3)))
p
ggsave("lin_regr_sed_corg_wetwt_DOME+EIONET.png", plot=p,
       width=20, height=20, units="cm",
       dpi=300)

# p<-ggplot(df2,aes(x=CORG,y=DRYWT))
# p<-p+geom_jitter(size=2,aes(colour=CORR))
# p<-p+geom_smooth(method='lm',formula=y~x,na.rm=T)
# p
# p<-ggplot(df2,aes(x=CORG,y=DRYWTinv))
# p<-p+geom_jitter(size=2,aes(colour=CORR))
# p<-p+geom_smooth(method='lm',formula=y~x,na.rm=T)
# p
# p<-ggplot(df2,aes(x=lnCORG,y=lnDRYWT))
# p<-p+geom_jitter(size=2,aes(colour=CORR))
# p<-p+geom_smooth(method='lm',formula=y~x,na.rm=T)
# p

# ---------------------------------------- AL vs CORG ---------------------------------------------

library(ggplot2)

df2<-df.sed %>%
  group_by(tblSampleID,AL,CORG) %>%
  summarise()
df2$lnAL<-log(df2$AL)
df2$lnCORG<-log(df2$CORG)
df2$CORR<-ifelse(df2$AL<0.1,ifelse(df2$lnCORG< -3,"2","1"),"0")

# plot DOME 
fit<-lm(formula=lnAL~lnCORG,data=df2)

p<-ggplot(df2,aes(x=lnCORG,y=lnAL))
p<-p+geom_jitter(size=2,aes(colour=CORR))
p<-p+ scale_color_grey() + theme_classic() #scale_color_manual(values=c("#000000", "#666666", "#AAAAAA"))
p<-p+geom_vline(xintercept=c(-3), linetype="dotted")
p<-p+geom_hline(yintercept=c(-2.302585), linetype="dotted")
p
ggsave("lin_regr_sed_corg_vs_AL_uncorrected.png", plot=p,width=20, height=20, units="cm",dpi=300)

df2$AL<-df2$AL*ifelse(df2$AL<0.1,100,1)
df2$CORG<-df2$CORG*ifelse(df2$CORR=="2",100,1)
df2$lnAL<-log(df2$AL)
df2$lnCORG<-log(df2$CORG)
fit<-lm(formula=lnAL~lnCORG,data=df2)

p<-ggplot(df2,aes(x=lnCORG,y=lnAL))
p<-p+geom_jitter(size=2,aes(colour=CORR))
p<-p+ scale_color_grey() + theme_classic()
p<-p+geom_smooth(method='lm',formula=y~x,na.rm=T)
p<-p+geom_vline(xintercept=c(-3), linetype="dotted")
p<-p+geom_hline(yintercept=c(-2.302585), linetype="dotted")
p<-p+labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
                        "Intercept =",signif(fit$coef[[1]],3 ),
                        " Slope =",signif(fit$coef[[2]], 3),
                        " P =",signif(summary(fit)$coef[2,4], 3)))
p
ggsave("lin_regr_sed_corg_vs_AL.png", plot=p,width=20, height=20, units="cm",dpi=300)


