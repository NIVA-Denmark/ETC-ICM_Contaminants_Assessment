

speciesavg<-df.biota %>%
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

df<-df.biota %>%
  select(Species,Common.Name,Type,MATRX,DRYWT,FATWT)
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


# try to sort in order of fatwt and drywt



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

