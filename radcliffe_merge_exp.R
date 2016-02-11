### Started 8 December 2015 ##
### By Ailene Ettinger ###
setwd("~/GitHub/radcliffe")
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(reshape)
library(zoo)
##Want to get several files:
#1. Phenology data: site, plot, event (phen event),year, doy,genus,species,Temp_treat, Precip_treat, onetempchange, precipchange, precipchangetyp
#2. Species names: site,genus,species,scrub,genus.prescrub,species.prescrub,ipni
#3. Study info: DatasetID  Year	medium	Temp_treat	onetempchange	Precip_treat	precipchange	precipchangetyp
#4. Temperature data
#5. Precipitation/moisture data

# make list to store all the derived dataset cleaning functions
clean.raw <- list()

clean.raw$marchin <- function(filename="Budburst_Marchin.csv", path="./Experiments/") {
  
  ## Marchin ##
  ## Data type: BBD,FFD ##
  ## Notes: Contact: Renee Marchin, renee.marchin@sydney.edu.au##
  file <- file.path(path, filename)
  marchin1 <- read.csv(file, check.names=FALSE, header=TRUE)
  names(marchin1)[2] <- "genusspecies"
  names(marchin1)[1] <- "year"
  names(marchin1)[3] <- "plot"
  names(marchin1)[8] <- "doy"
  marchin1a<- subset(marchin1, select=c("year","genusspecies","plot", "doy"))
  marchin1a$site <- "marchin"
  marchin1a$event <- "bbd"
  marchin2<-read.csv("Experiments/Flower_Marchin.csv", header=T)
  names(marchin2)[2] <- "genusspecies"
  names(marchin2)[1] <- "year"
  names(marchin2)[3] <- "plot"
  names(marchin2)[7] <- "doy"  
  marchin2a<- subset(marchin2, select=c("year","genusspecies","plot", "doy"))
  marchin2a$site <- "marchin"
  marchin2a$event <- "ffd"
  marchin3<-rbind(marchin1a, marchin2a)
  marchin3$genus<-NA
  marchin3$species<-NA
  marchin3$genus[marchin3$genusspecies=="ACRU"] <- "Acer"
  marchin3$species[marchin3$genusspecies=="ACRU"] <- "rubrum"
  marchin3$genus[marchin3$genusspecies=="CATO"] <- "Carya"
  marchin3$species[marchin3$genusspecies=="CATO"] <- "tomentosa"
  marchin3$genus[marchin3$genusspecies=="QUAL"] <- "Quercus"
  marchin3$species[marchin3$genusspecies=="QUAL"] <- "alba"
  marchin3$genus[marchin3$genusspecies=="VAPA"] <- "Vaccinium"
  marchin3$species[marchin3$genusspecies=="VAPA"] <- "pallidum"
  marchin3$genus[marchin3$genusspecies=="VAST"] <- "Vaccinium"
  marchin3$species[marchin3$genusspecies=="VAST"] <- "stamineum"
  marchin3$genus[marchin3$genusspecies=="QURU"] <- "Quercus"
  marchin3$species[marchin3$genusspecies=="QURU"] <- "rubra"
  marchin3$genus[marchin3$genusspecies=="CHMA"] <- "Chimaphila"
  marchin3$species[marchin3$genusspecies=="CHMA"] <- "maculata"
  marchin3$genus[marchin3$genusspecies=="HEAR"] <- "Hexastylis"
  marchin3$species[marchin3$genusspecies=="HEAR"] <- "arifolia"
  marchin3$genus[marchin3$genusspecies=="HIVE"] <- "Hieracium"
  marchin3$species[marchin3$genusspecies=="HIVE"] <- "venosum"
  marchin3$genus[marchin3$genusspecies=="THTH"] <- "Thalictrum"
  marchin3$species[marchin3$genusspecies=="THTH"] <- "thalictroides"
  marchin3$genus[marchin3$genusspecies=="TIDI"] <- "Tipularia"
  marchin3$species[marchin3$genusspecies=="TIDI"] <- "discolor"
  marchin<-subset(marchin3, select=c("site","plot","event","year","genus","species", "doy"))
  marchin$variety <- NA
  marchin$cult <- NA
  
  return(marchin)
}

clean.raw$bace <- function(filename="BACE_deciduous2010_originaltrees.csv", path="./Experiments",names.only=FALSE) {  
  ##BACE ##
  ## Data type: BBD,LOD ##
  ## Notes: Jeff Dukes##
  ##Decided to follow NPN's definitios: >3 of observations of each event needed to count
  file <- file.path(path, filename)
  bace1 <- read.csv(file, check.names=FALSE, header=TRUE)
  bace1<-bace1[-1,]
  names(bace1)[5] <- "genusspecies"
  names(bace1)[1] <- "plot"
  names(bace1)[7] <- "doy_bb"
  names(bace1)[9] <- "doy_lunf"
  names(bace1)[10] <- "doy_lo"
  names(bace1)[9] <- "doy_lunf"
  bace1a<- subset(bace1, select=c("genusspecies","plot", "doy_bb"))
  bace1a$event <- "bbd"
  bace1a$year <- 2010
  bace1a$site <- "bace"
  head(bace1a)
  names(bace1a)[3]<-"doy"
  bace2a<- subset(bace1, select=c("genusspecies","plot", "doy_lo"))
  bace2a$event <- "lod"
  bace2a$year <- 2010
  bace2a$site <- "bace"
  names(bace2a)[3]<-"doy"
  bace2b<- subset(bace1, select=c("genusspecies","plot", "doy_lunf"))
  bace2b$event <- "lud"
  bace2b$year <- 2010
  bace2b$site <- "bace"
  names(bace2b)[3]<-"doy"
  bace3<-rbind(bace1a, bace2a,bace2b)
  bace3$genus<-NA
  bace3$species<-NA
  bace3$genus[bace3$genusspecies=="A. rubrum "] <- "Acer"
  bace3$species[bace3$genusspecies=="A. rubrum "] <- "rubrum"
  bace3$genus[bace3$genusspecies=="A. rubrum  "] <- "Acer"
  bace3$species[bace3$genusspecies=="A. rubrum  "] <- "rubrum"
  bace3$genus[bace3$genusspecies=="A. rubrum (main stem)"] <- "Acer"
  bace3$species[bace3$genusspecies=="A. rubrum (main stem)"] <- "rubrum"
  bace3$genus[bace3$genusspecies=="B. lenta  "] <- "Betula"
  bace3$species[bace3$genusspecies=="B. lenta  "] <- "lenta"
  bace3$genus[bace3$genusspecies=="B. lenta   "] <- "Betula"
  bace3$species[bace3$genusspecies=="B. lenta   "] <- "lenta"
  bace3$genus[bace3$genusspecies=="B. lenta (main stem)"] <- "Betula"
  bace3$species[bace3$genusspecies=="B. lenta (main stem)"] <- "lenta"
  bace3$genus[bace3$genusspecies=="Q. rubra  "] <- "Quercus"
  bace3$species[bace3$genusspecies=="Q. rubra  "] <- "rubra"
  bace3$genus[bace3$genusspecies=="Q. rubra   "] <- "Quercus"
  bace3$species[bace3$genusspecies=="Q. rubra   "] <- "rubra"
  bace<-subset(bace3, select=c("site","plot","event","year","genus","species", "doy"))
  bace$variety <- NA
  bace$cult <- NA
  return(bace)
}

##Farnsworth from Harvard ##
## Data type: BBD,LOD,LUD,FFD ##
## Notes: Contact: Public data, http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf033 ##
clean.raw$farnsworth <- function(filename="hf033-01-diameter-1.csv", path="./Experiments",names.only=FALSE) {
  file <- file.path(path, filename)
  farnsworth1 <- read.csv(file, check.names=FALSE, header=TRUE)
  #phenological stage 1.5=budburst; need to get day of year for which this occurred
  farnsworth1$plot<-paste(farnsworth1$treat,farnsworth1$block,farnsworth1$quad,sep=".")
  farnsworth1$bb_doy<-NA
  for(i in 1:dim(farnsworth1)[1]){
    inddat<-farnsworth1[i,20:31]
    names(inddat)[1:12]<-c("1993-04-16","1993-04-23","1993-05-2","1993-05-17","1993-05-24" ,"1993-06-07","1993-07-09","1993-07-23","1993-08-11","1993-09-09","1993-09-25","1993-10-23")
    bbdate<-names(inddat)[min(which(inddat>1))]
    bbdoy<-strftime(bbdate, format = "%j")
    farnsworth1$bb_doy[i]<-bbdoy
  }
  #now phenological stage 2=leaves expanding; need to get day of year for which this occurred
  farnsworth1$leafunf_doy<-NA
  for(i in 1:dim(farnsworth1)[1]){
    inddat<-farnsworth1[i,20:31]
    names(inddat)[1:12]<-c("1993-04-16","1993-04-23","1993-05-2","1993-05-17","1993-05-24" ,"1993-06-07","1993-07-09","1993-07-23","1993-08-11","1993-09-09","1993-09-25","1993-10-23")
    ludate<-names(inddat)[min(which(inddat>1.5))]
    ludoy<-strftime(ludate, format = "%j")
    farnsworth1$leafunf_doy[i]<-ludoy
  }
  #now phenological stage 3=leaves fully expanded=leafout; need to get day of year for which this occurred
  farnsworth1$leafout_doy<-NA
  for(i in 1:dim(farnsworth1)[1]){
    inddat<-farnsworth1[i,20:31]
    names(inddat)[1:12]<-c("1993-04-16","1993-04-23","1993-05-2","1993-05-17","1993-05-24" ,"1993-06-07","1993-07-09","1993-07-23","1993-08-11","1993-09-09","1993-09-25","1993-10-23")
    lodate<-names(inddat)[min(which(inddat>2))]
    lodoy<-strftime(lodate, format = "%j")
    farnsworth1$leafout_doy[i]<-lodoy
  }
  #now flowering phenological stage (4.5)=mature leaves and flowers present;first flowering=day of year for which this first occurred
  farnsworth1$ffd<-NA
  for(i in 1:dim(farnsworth1)[1]){
    inddat<-farnsworth1[i,20:31]
    names(inddat)[1:12]<-c("1993-04-16","1993-04-23","1993-05-2","1993-05-17","1993-05-24" ,"1993-06-07","1993-07-09","1993-07-23","1993-08-11","1993-09-09","1993-09-25","1993-10-23")
    ffdate<-names(inddat)[min(which(inddat==4.5))]
    ffdoy<-strftime(ffdate, format = "%j")
    farnsworth1$ffd[i]<-ffdoy
  }
  #now fruiting phenological stage (5)=mature leaves and fruits present=first fruiting is first date this was observed
  farnsworth1$ffrd<-NA
  for(i in 1:dim(farnsworth1)[1]){
    inddat<-farnsworth1[i,20:31]
    names(inddat)[1:12]<-c("1993-04-16","1993-04-23","1993-05-2","1993-05-17","1993-05-24" ,"1993-06-07","1993-07-09","1993-07-23","1993-08-11","1993-09-09","1993-09-25","1993-10-23")
    ffrdate<-names(inddat)[min(which(inddat==5))]
    ffrdoy<-strftime(ffrdate, format = "%j")
    farnsworth1$ffrd[i]<-ffrdoy
  }
  farnsworth1$genus<-NA
  farnsworth1$species1<-NA
  farnsworth1$genus[farnsworth1$species=="aaga"] <- "Amelanchier"
  farnsworth1$species1[farnsworth1$species=="aaga"] <- "grandifolia"
  farnsworth1$genus[farnsworth1$species=="beech"] <- "Fagus"
  farnsworth1$species1[farnsworth1$species=="beech"] <- "grandifolia"
  farnsworth1$genus[farnsworth1$species=="bbhg"] <- "Vaccinium"
  farnsworth1$species1[farnsworth1$species=="bbhg"] <- "corymbosum"
  farnsworth1$genus[farnsworth1$species=="bbhch"] <- "Vaccinium"
  farnsworth1$species1[farnsworth1$species=="bbhch"] <- "vacillans"
  farnsworth1$genus[farnsworth1$species=="blach"] <- "Prunus"
  farnsworth1$species1[farnsworth1$species=="blach"] <- "serotina"
  farnsworth1$genus[farnsworth1$species=="crata"] <- "Acer"
  farnsworth1$species1[farnsworth1$species=="crata"] <- "rubrum"
  farnsworth1$genus[farnsworth1$species=="ro"] <- "Quercus"
  farnsworth1$species1[farnsworth1$species=="ro"] <- "rubra"
  farnsworth1$genus[farnsworth1$species=="sa"] <- "Sorbus"
  farnsworth1$species1[farnsworth1$species=="sa"] <- "americana"
  farnsworth1$genus[farnsworth1$species=="wo"] <- "Quercus"
  farnsworth1$species1[farnsworth1$species=="wo"] <- "alba"
  farnsworth1$genus[farnsworth1$species=="viac1"] <- "Viburnum"
  farnsworth1$species1[farnsworth1$species=="viac1"] <- "acerifolium"
  farnsworth1$genus[farnsworth1$species=="sm"] <- "Acer"
  farnsworth1$species1[farnsworth1$species=="sm"] <- "pensylvanicum"
  farnsworth1$genus[farnsworth1$species=="chest"] <- "Castanea"
  farnsworth1$species1[farnsworth1$species=="chest"] <- "dentata"
  farnsworth1$genus[farnsworth1$species=="vest"] <- "Viburnum"
  farnsworth1$species1[farnsworth1$species=="vest"] <- "lentago"
  farnsworth1$genus[farnsworth1$species=="rm"] <- "Acer"
  farnsworth1$species1[farnsworth1$species=="rm"] <- "rubrum"
  farnsworth1$site<-"farnsworthharv"  
  farnsworth1$variety <- NA
  farnsworth1$cult <- NA
  farnsworth1$event <- NA
  farnsworth1$year <- 1993
  #pull out all budburst rows
  farnsworth2<-farnsworth1[which(farnsworth1$bb_doy>0),]
  farnsworth2a<-subset(farnsworth2, select=c("site","plot","event","year","genus","species1","bb_doy","variety","cult"))
  colnames(farnsworth2a)[6]<-"species"
  colnames(farnsworth2a)[7]<-"doy"
  farnsworth2a$event <- "bbd"
  #pull out all leafunf rows
  farnsworth3<-farnsworth1[which(farnsworth1$leafunf_doy>0),]
  farnsworth3a<-subset(farnsworth3, select=c("site","plot","event","year","genus","species1","bb_doy","variety","cult"))
  colnames(farnsworth3a)[6]<-"species"
  colnames(farnsworth3a)[7]<-"doy"
  farnsworth3a$event <- "lud"
  #pull out all leafout rows
  farnsworth4<-farnsworth1[which(farnsworth1$leafout_doy>0),]
  farnsworth4a<-subset(farnsworth4, select=c("site","plot","event","year","genus","species1","bb_doy","variety","cult"))
  colnames(farnsworth4a)[6]<-"species"
  colnames(farnsworth4a)[7]<-"doy"
  farnsworth4a$event <- "lod"
  #pull out all flowering rows
  farnsworth5<-farnsworth1[which(farnsworth1$ffd>0),]
  farnsworth5a<-subset(farnsworth5, select=c("site","plot","event","year","genus","species1","bb_doy","variety","cult"))
  colnames(farnsworth5a)[6]<-"species"
  colnames(farnsworth5a)[7]<-"doy"
  farnsworth5a$event <- "ffd"
  #pull out all fruiting rows
  farnsworth6<-farnsworth1[which(farnsworth1$ffrd>0),]
  farnsworth6a<-subset(farnsworth6, select=c("site","plot","event","year","genus","species1","bb_doy","variety","cult"))
  colnames(farnsworth6a)[6]<-"species"
  colnames(farnsworth6a)[7]<-"doy"
  farnsworth6a$event <- "ffrd"
  farnsworth<- rbind(farnsworth2a,farnsworth3a,farnsworth3a,farnsworth4a,farnsworth5a,farnsworth6a)
  return(farnsworth)
}
###Cleland et al Jasper Ridge data
###FFD
clean.raw$jasperridge <- function(filename="JasperRidge_data.csv", path="./Experiments/") {
  
  file <- file.path(path, filename)
  cleland1 <- read.csv(file, check.names=FALSE, header=TRUE)  
  colnames(cleland1)[8]<-"genus"
  cleland1$species<-NA
  cleland1$species[cleland1$genus=="Crepis"] <- "vessicaria"
  cleland1$species[cleland1$genus=="Erodium"] <- "brachycarpum"
  cleland1$species[cleland1$genus=="Geranium"] <- "dissectum"
  cleland1$species[cleland1$genus=="Lolium"] <- "multiflorum"
  cleland1$species[cleland1$genus=="Vicia"] <- "sativa"
  cleland1$species[cleland1$genus=="Vulpia"] <- "myuros"
  cleland1$species[cleland1$genus=="Bromusd"] <- "diandrus"
  cleland1$species[cleland1$genus=="Bromush"] <- "hordeaceus"
  cleland1$genus[cleland1$genus=="Bromusd"] <- "Bromus"
  cleland1$genus[cleland1$genus=="Bromush"] <- "Bromus"
  colnames(cleland1)[10]<-"doy"
  cleland1$site<-"jasper"
  cleland1$event<-"ffd"
  cleland<-subset(cleland1, select=c("site","plot","event","year","genus","species", "doy"))
  cleland$variety <- NA
  cleland$cult <- NA
  return(cleland)
}



##Clark et al from Duke ##
## Data type: BBD,LUD, LOD ##
## Notes: Contact: Public data ##

clean.raw$clarkduke <- function(filename, path) {
  clarkdukeplots<-c("DF_G01_A.csv","DF_G02_5.csv","DF_G03_3.csv","DF_G04_A.csv","DF_G05_3.csv","DF_G06_5.csv","DF_G07_A.csv","DF_G08_5.csv","DF_G09_3.csv","DF_G10_C.csv","DF_G11_C.csv","DF_G12_C.csv","DF_S01_5.csv","DF_S02_3.csv","DF_S03_A.csv","DF_S04_A.csv","DF_S05_3.csv","DF_S06_5.csv","DF_S07_5.csv","DF_S08_A.csv","DF_S09_3.csv","DF_S10_C.csv","DF_S11_C.csv","DF_S12_C.csv")
  clarkduke <- NA
  spfile <- file.path(path, "speciesList_clark.csv")
  specieslist<-read.csv(spfile, header=TRUE)
  for (i in 1:length(clarkdukeplots)){
  file <- file.path(path, paste(clarkdukeplots[i]))
  clarkduke1 <- read.csv(file, check.names=FALSE, header=TRUE)
  clarkduke1$genus<-NA
  clarkduke1$species<-NA
  species1<-unique(clarkduke1$Species)
  for (j in 1:length(species1)){
  clarkduke1$genus[clarkduke1$Species==species1[j]] <- specieslist[specieslist$shortCode==species1[j],]$genus
  clarkduke1$species[clarkduke1$Species==species1[j]] <- specieslist[specieslist$shortCode==species1[j],]$species
  }
clarkduke1$site<-"duke_clark"
colnames(clarkduke1)[5]<-"plot"

#estimate first date of budburst, leaf unfolding, and leaf out
get.bbd <- function(x) names(x)[min(which(x==3), na.rm=T)]#budburst
get.lud <- function(x) names(x)[min(which(x==4), na.rm=T)]#leaves unfolding
get.lod <- function(x) names(x)[min(which(x==6), na.rm=T)]#leafout
bbd_2010<-substr(apply(clarkduke1[,17:30],1,get.bbd),6,13)
bbd_2011<-substr(apply(clarkduke1[,31:55],1,get.bbd),6,13)
bbd_2012<-substr(apply(clarkduke1[,56:81],1,get.bbd),6,13)
bbd_2013<-substr(apply(clarkduke1[,82:101],1,get.bbd),6,13)
lud_2010<-substr(apply(clarkduke1[,17:30],1,get.lud),6,13)
lud_2011<-substr(apply(clarkduke1[,31:55],1,get.lud),6,13)
lud_2012<-substr(apply(clarkduke1[,56:81],1,get.lud),6,13)
lud_2013<-substr(apply(clarkduke1[,82:101],1,get.lud),6,13)
lod_2010<-substr(apply(clarkduke1[,17:30],1,get.lod),6,13)
lod_2011<-substr(apply(clarkduke1[,31:55],1,get.lod),6,13)
lod_2012<-substr(apply(clarkduke1[,56:81],1,get.lod),6,13)
lod_2013<-substr(apply(clarkduke1[,82:101],1,get.lod),6,13)
bbd2010_doy<-strftime(strptime(bbd_2010, format = "%m.%d.%y"),format = "%j")
bbd2011_doy<-strftime(strptime(bbd_2011, format = "%m.%d.%y"),format = "%j")
bbd2012_doy<-strftime(strptime(bbd_2012, format = "%m.%d.%y"),format = "%j")
bbd2013_doy<-strftime(strptime(bbd_2013, format = "%m.%d.%y"),format = "%j")
lud2010_doy<-strftime(strptime(lud_2010, format = "%m.%d.%y"),format = "%j")
lud2011_doy<-strftime(strptime(lud_2011, format = "%m.%d.%y"),format = "%j")
lud2012_doy<-strftime(strptime(lud_2012, format = "%m.%d.%y"),format = "%j")
lud2013_doy<-strftime(strptime(lud_2013, format = "%m.%d.%y"),format = "%j")
lod2010_doy<-strftime(strptime(lod_2010, format = "%m.%d.%y"),format = "%j")
lod2011_doy<-strftime(strptime(lod_2011, format = "%m.%d.%y"),format = "%j")
lod2012_doy<-strftime(strptime(lod_2012, format = "%m.%d.%y"),format = "%j")
lod2013_doy<-strftime(strptime(lod_2013, format = "%m.%d.%y"),format = "%j")
clarkduke2<-cbind(clarkduke1,bbd2010_doy,bbd2011_doy,bbd2012_doy,bbd2013_doy,lud2010_doy,lud2011_doy,lud2012_doy,lud2013_doy,lod2010_doy,lod2011_doy,lod2012_doy,lod2013_doy)
clarkduke2a<-subset(clarkduke2, select=c("site","plot","genus","species","bbd2010_doy","bbd2011_doy","bbd2012_doy","bbd2013_doy","lud2010_doy","lud2011_doy","lud2012_doy","lud2013_doy","lod2010_doy","lod2011_doy","lod2012_doy","lod2013_doy"))
clarkduke3<-reshape(clarkduke2a,varying = list(names(clarkduke2a)[5:8], names(clarkduke2a)[9:12],names(clarkduke2a)[13:16]), direction = "long", v.names = c("BBD","LUD", "LOD"), times = c(2010:2013))
clarkduke3<-clarkduke3[,-9]
colnames(clarkduke3)[5]<-"year"
clarkduke4<-reshape(clarkduke3,varying = list(names(clarkduke3)[6:8]), direction = "long", v.names = c("doy"), times = c(1:3))
clarkduke4$event<-c(rep("bbd", times=dim(clarkduke3)[1]),rep("lud", times=dim(clarkduke3)[1]),rep("lod", times=dim(clarkduke3)[1]))
clarkduke4$variety <- NA
clarkduke4$cult <- NA
clarkduke5<-subset(clarkduke4, select=c("site","plot","event","year","genus","species","doy","variety","cult"))
clarkduke<-rbind(clarkduke,clarkduke5)
}
clarkduke<-clarkduke[-1,]
return(clarkduke)
}

##Clark et al from Harvard ##
## Data type: BBD,LUD,LOD ##
## Notes: Contact: Public data ##

clean.raw$clarkharvard <- function(filename, path) {
    clarkharvardplots<-c("HF_G01_3.csv","HF_G02_A.csv","HF_G03_5.csv","HF_G04_A.csv","HF_G05_5.csv","HF_G06_3.csv","HF_G07_A.csv","HF_G08_3.csv","HF_G09_5.csv","HF_G10_C.csv","HF_G11_C.csv","HF_G12_C.csv","HF_S01_5.csv","HF_S02_A.csv","HF_S03_3.csv","HF_S04_5.csv","HF_S05_A.csv","HF_S06_3.csv","HF_S07_A.csv","HF_S08_3.csv","HF_S09_5.csv","HF_S10_C.csv","HF_S11_C.csv","HF_S12_C.csv")
    clarkharvard <- NA
    spfile <- file.path(path, "speciesList_clark.csv")
    specieslist<-read.csv(spfile, header=TRUE)
    for (i in 1:length(clarkharvardplots)){
      file <- file.path(path, paste(clarkharvardplots[i]))
      clarkharvard1 <- read.csv(file, check.names=FALSE, header=TRUE)
      clarkharvard1$genus<-NA
      clarkharvard1$species<-NA
      species1<-unique(clarkharvard1$Species)
      for (j in 1:length(species1)){
        clarkharvard1$genus[clarkharvard1$Species==species1[j]] <- specieslist[specieslist$shortCode==species1[j],]$genus
        clarkharvard1$species[clarkharvard1$Species==species1[j]] <- specieslist[specieslist$shortCode==species1[j],]$species
      }
      clarkharvard1$site<-"harvard_clark"
      colnames(clarkharvard1)[5]<-"plot"
      
      #estimate first date of budburst, leaf unfolding, and leaf out
      get.bbd <- function(x) names(x)[min(which(x==3), na.rm=T)]#budburst
      get.lud <- function(x) names(x)[min(which(x==4), na.rm=T)]#leaves unfolding
      get.lod <- function(x) names(x)[min(which(x==6), na.rm=T)]#leafout
      bbd_2010<-substr(apply(clarkharvard1[,19:26],1,get.bbd),6,13)
      bbd_2011<-substr(apply(clarkharvard1[,27:31],1,get.bbd),6,13)
      bbd_2012<-substr(apply(clarkharvard1[,32:44],1,get.bbd),6,13)
      lud_2010<-substr(apply(clarkharvard1[,19:26],1,get.lud),6,13)
      lud_2011<-substr(apply(clarkharvard1[,27:31],1,get.lud),6,13)
      lud_2012<-substr(apply(clarkharvard1[,32:44],1,get.lud),6,13)
      lod_2010<-substr(apply(clarkharvard1[,19:26],1,get.lod),6,13)
      lod_2011<-substr(apply(clarkharvard1[,27:31],1,get.lod),6,13)
      lod_2012<-substr(apply(clarkharvard1[,32:44],1,get.lod),6,13)
      bbd2010_doy<-strftime(strptime(bbd_2010, format = "%m.%d.%y"),format = "%j")
      bbd2011_doy<-strftime(strptime(bbd_2011, format = "%m.%d.%y"),format = "%j")
      bbd2012_doy<-strftime(strptime(bbd_2012, format = "%m.%d.%y"),format = "%j")
      lud2010_doy<-strftime(strptime(lud_2010, format = "%m.%d.%y"),format = "%j")
      lud2011_doy<-strftime(strptime(lud_2011, format = "%m.%d.%y"),format = "%j")
      lud2012_doy<-strftime(strptime(lud_2012, format = "%m.%d.%y"),format = "%j")
      lod2010_doy<-strftime(strptime(lod_2010, format = "%m.%d.%y"),format = "%j")
      lod2011_doy<-strftime(strptime(lod_2011, format = "%m.%d.%y"),format = "%j")
      lod2012_doy<-strftime(strptime(lod_2012, format = "%m.%d.%y"),format = "%j")
      clarkharvard2<-cbind(clarkharvard1,bbd2010_doy,bbd2011_doy,bbd2012_doy,lud2010_doy,lud2011_doy,lud2012_doy,lod2010_doy,lod2011_doy,lod2012_doy)
      clarkharvard2a<-subset(clarkharvard2, select=c("site","plot","genus","species","bbd2010_doy","bbd2011_doy","bbd2012_doy","lud2010_doy","lud2011_doy","lud2012_doy","lod2010_doy","lod2011_doy","lod2012_doy"))
      clarkharvard3<-reshape(clarkharvard2a,varying = list(names(clarkharvard2a)[5:7], names(clarkharvard2a)[8:10],names(clarkharvard2a)[11:13]), direction = "long", v.names = c("BBD","LUD", "LOD"), times = c(2010:2012))
      clarkharvard3<-clarkharvard3[,-9]
      colnames(clarkharvard3)[5]<-"year"
      clarkharvard4<-reshape(clarkharvard3,varying = list(names(clarkharvard3)[6:8]), direction = "long", v.names = c("doy"), times = c(1:3))
      clarkharvard4$event<-c(rep("bbd", times=dim(clarkharvard3)[1]),rep("lud", times=dim(clarkharvard3)[1]),rep("lod", times=dim(clarkharvard3)[1]))
      clarkharvard4$variety <- NA
      clarkharvard4$cult <- NA
      clarkharvard5<-subset(clarkharvard4, select=c("site","plot","event","year","genus","species","doy","variety","cult"))
      clarkharvard<-rbind(clarkharvard,clarkharvard5)
    }
    clarkharvard<-clarkharvard[-1,]
    return(clarkharvard)
  }
  
  


# Produce cleaned raw data
#
raw.data.dir <- "./Experiments/"
cleandata.raw <- list()
cleandata.raw$marchin <- clean.raw$marchin(path=raw.data.dir)
cleandata.raw$bace <- clean.raw$bace(path=raw.data.dir)
cleandata.raw$farnsworth <- clean.raw$farnsworth(path=raw.data.dir)
cleandata.raw$jasperridge <- clean.raw$jasperridge(path=raw.data.dir)
cleandata.raw$clarkduke <- clean.raw$clarkduke("DF_G01_A.csv",raw.data.dir)
cleandata.raw$clarkharvard <- clean.raw$clarkharvard("HF_G01_A.csv",raw.data.dir)
