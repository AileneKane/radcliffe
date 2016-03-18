### Started 8 December 2015 ##
### By Ailene Ettinger ###
setwd("~/GitHub/radcliffe") # setwd("~/Documents/git/projects/meta_ep2/radcliffe")
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
clean.raw$marchin <- function(filename="Budburst_Marchin.csv", path="./Experiments/marchin") {
  
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
  marchin2<-read.csv("Experiments/marchin/Flower_Marchin.csv", header=T)
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
  #marchin$variety <- NA
  #marchin$cult <- NA
  return(marchin)
}

clean.raw$bace <- function(filename="BACE_deciduous2010_originaltrees.csv", path="./Experiments/bace",names.only=FALSE) {  
  ##BACE ##
  ## Data type: BBD,LOD, LUD ##
  ## Notes: Jeff Dukes##
  ##Decided to follow NPN's definitios: >3 of observations of each event needed to count
  file <- file.path(path, filename)
  bace1 <- read.csv(file, check.names=FALSE, header=TRUE,na.strings = ".")
  bace1<-bace1[-1,]
  names(bace1)[5] <- "genusspecies"
  names(bace1)[1] <- "plot"
  names(bace1)[7] <- "doy_bb"
  names(bace1)[9] <- "doy_lunf"
  names(bace1)[10] <- "doy_lo"
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
  file2 <- file.path(path, "BACE_pinustrobus2010_originaltrees.csv")
  #bace4 <- read.csv(file2, check.names=FALSE, header=TRUE,na.strings = ".")
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
  #bace$variety <- NA
  #bace$cult <- NA
  bace<-bace[!is.na(bace$genus),]
  bace<-bace[!is.na(bace$doy),]
  bace<-bace[-which(bace$doy==""),]
  return(bace)
}

##Farnsworth from Harvard ##
## Data type: BBD,LOD,LUD,FFD ##
## Notes: Contact: Public data, http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf033 ##
##Question: hf033-01-diameter-1.csv files says plot 17= intact control (Treat=1) but soil temp file says plot17=disturbance control (=d)
clean.raw$farnsworth <- function(filename="hf033-01-diameter-1.csv", path="./Experiments/farnsworth/",names.only=FALSE) {
  file <- file.path(path, filename)
  farnsworth1 <- read.csv(file, check.names=FALSE, header=TRUE)
  #phenological stage 1.5=budburst; need to get day of year for which this occurred
  colnames(farnsworth1)[3]<-"plot"
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
  #farnsworth1$variety <- NA
  #farnsworth1$cult <- NA
  farnsworth1$event <- NA
  farnsworth1$year <- 1993
  #pull out all budburst rows
  farnsworth2<-farnsworth1[which(farnsworth1$bb_doy>0),]
  farnsworth2a<-subset(farnsworth2, select=c("site","plot","event","year","genus","species1","bb_doy"))
  colnames(farnsworth2a)[6]<-"species"
  colnames(farnsworth2a)[7]<-"doy"
  farnsworth2a$event <- "bbd"
  #pull out all leafunf rows
  farnsworth3<-farnsworth1[which(farnsworth1$leafunf_doy>0),]
  farnsworth3a<-subset(farnsworth3, select=c("site","plot","event","year","genus","species1","bb_doy"))
  colnames(farnsworth3a)[6]<-"species"
  colnames(farnsworth3a)[7]<-"doy"
  farnsworth3a$event <- "lud"
  #pull out all leafout rows
  farnsworth4<-farnsworth1[which(farnsworth1$leafout_doy>0),]
  farnsworth4a<-subset(farnsworth4, select=c("site","plot","event","year","genus","species1","bb_doy"))
  colnames(farnsworth4a)[6]<-"species"
  colnames(farnsworth4a)[7]<-"doy"
  farnsworth4a$event <- "lod"
  #pull out all flowering rows
  farnsworth5<-farnsworth1[which(farnsworth1$ffd>0),]
  farnsworth5a<-subset(farnsworth5, select=c("site","plot","event","year","genus","species1","bb_doy"))
  colnames(farnsworth5a)[6]<-"species"
  colnames(farnsworth5a)[7]<-"doy"
  farnsworth5a$event <- "ffd"
  #pull out all fruiting rows
  farnsworth6<-farnsworth1[which(farnsworth1$ffrd>0),]
  farnsworth6a<-subset(farnsworth6, select=c("site","plot","event","year","genus","species1","bb_doy"))
  colnames(farnsworth6a)[6]<-"species"
  colnames(farnsworth6a)[7]<-"doy"
  farnsworth6a$event <- "ffrd"
  farnsworth<- rbind(farnsworth2a,farnsworth3a,farnsworth3a,farnsworth4a,farnsworth5a,farnsworth6a)
  return(farnsworth)
}
###Cleland et al Jasper Ridge data
###FFD
clean.raw$jasper <- function(filename="JasperRidge_data.csv", path="./Experiments/jasper") {
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
  #cleland$variety <- NA
  #cleland$cult <- NA
  cleland<-cleland[!is.na(cleland$doy),]
  
  return(cleland)
}



##Clark et al from Duke ##
## Data type: BBD,LUD, LOD ##
## Notes: Contact: Public data ##
clean.raw$clarkduke <- function(filename, path="./Experiments/clark/") {
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
clarkduke1$site<-"clarkduke"
clarkduke1$plot<-substr(clarkduke1$Chamber,1,3)

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
#clarkduke4$variety <- NA
#clarkduke4$cult <- NA
clarkduke5<-subset(clarkduke4, select=c("site","plot","event","year","genus","species","doy"))
clarkduke<-rbind(clarkduke,clarkduke5)
}
clarkduke<-clarkduke[-1,]
clarkduke<-clarkduke[!is.na(clarkduke$doy),]

return(clarkduke)
}

##Clark et al from Harvard ##
## Data type: BBD,LUD,LOD ##
## Notes: Contact: Public data ##
clean.raw$clarkharvard <- function(filename, path="./Experiments/clark") {
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
      clarkharvard1$site<-"clarkharvard"      
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
      clarkharvard2$plot<-substr(clarkharvard1$Chamber,1,3)
      clarkharvard2a<-subset(clarkharvard2, select=c("site","plot","genus","species","bbd2010_doy","bbd2011_doy","bbd2012_doy","lud2010_doy","lud2011_doy","lud2012_doy","lod2010_doy","lod2011_doy","lod2012_doy"))
      clarkharvard3<-reshape(clarkharvard2a,varying = list(names(clarkharvard2a)[5:7], names(clarkharvard2a)[8:10],names(clarkharvard2a)[11:13]), direction = "long", v.names = c("BBD","LUD", "LOD"), times = c(2010:2012))
      clarkharvard3<-clarkharvard3[,-9]
      colnames(clarkharvard3)[5]<-"year"
      clarkharvard4<-reshape(clarkharvard3,varying = list(names(clarkharvard3)[6:8]), direction = "long", v.names = c("doy"), times = c(1:3))
      clarkharvard4$event<-c(rep("bbd", times=dim(clarkharvard3)[1]),rep("lud", times=dim(clarkharvard3)[1]),rep("lod", times=dim(clarkharvard3)[1]))
      #clarkharvard4$variety <- NA
      #clarkharvard4$cult <- NA
      
      clarkharvard5<-subset(clarkharvard4, select=c("site","plot","event","year","genus","species","doy"))
      clarkharvard<-rbind(clarkharvard,clarkharvard5)
    }
    clarkharvard<-clarkharvard[-1,]
    clarkharvard<-clarkharvard[!is.na(clarkharvard$doy),]
    return(clarkharvard)
  }
  
##Sherry from Oklahoma##
## Data type: FFD, FFRD ##
## Notes: Rebecca Sherry
#Phenological stages for Forbs: F0=vegetative plants; F1, unopened buds; F2, open flowers; F3, old flowers (postanthesis); F4, initiated fruit; F5,expanding fruit; and F6, dehisced fruit. 
#Phenological stages For grasses: G0, plants with flower stalks (in boot); G1, spikelets present (out of boot); G2,exerted anthers or styles; G3, past the presence of anthers and styles (seed development); and G4, disarticulating florets. 
#For forb species with very small flowers and fruits that were difficult to observe, stage 3 (initiated fruit) and stage 4 (expanding fruit) were lumped into a category of ‘‘fruit present,’’ (i.e., a score of F4.5)
clean.raw$sherry <- function(filename, path) {
  sherryspp<-c("SherryPhenology2003_Achillea.csv","SherryPhenology2003_Ambrosia.csv","SherryPhenology2003_Andropogon.csv","SherryPhenology2003_Erigeron.csv","SherryPhenology2003_Panicum.csv","SherryPhenology2003_Schizachyrium.csv")
  sherry <- NA
  gen<-c("Achillea","Ambrosia","Andropogon","Erigeron","Panicum","Schizachyrium")
  sp<-c("millefolium","psilostchya","gerardii","strigosus","virgatum","scoparium")
  for (i in 1:length(sherryspp)){
    file <- file.path(path, paste(sherryspp[i]))
    sherry1 <- read.csv(file, skip=3, header=TRUE)
    colnames(sherry1)[which(colnames(sherry1)=="Plot")]<-"plot"
  #estimate first date of budburst, leaf unfolding, and leaf out
  firstsurv<-min(which(substr(colnames(sherry1),1,1)=="X"))
  lastsurv<-dim(sherry1)[2]  
  get.ffd <- function(x) names(x)[min(which(x <= 3.5 & x >= 2.5), na.rm=T)]#first flower date
  get.ffrd <- function(x) names(x)[min(which(x <= 5.5 & x >= 3.5), na.rm=T)]#leaves unfolding
  ffd_doy<-substr(apply(sherry1[,firstsurv:lastsurv],1,get.ffd),2,4)
  ffrd_doy<-substr(apply(sherry1[,firstsurv:lastsurv],1,get.ffrd),2,4)
  sherry2<-cbind(sherry1,ffd_doy,ffrd_doy)
  sherry2$genus<- paste(gen[i])
  sherry2$species<-paste(sp[i])    
  sherry3<-subset(sherry2, select=c("plot","genus","species", "ffd_doy","ffrd_doy"))
  sherry<-rbind(sherry,sherry3)
  }
  sherry<-sherry[-1,]
  sherry4<-reshape(sherry,varying = list(names(sherry)[4:5]), direction = "long", v.names = c("doy"), times = c(1:2))
  sherry4$event<-c(rep("ffd", times=dim(sherry)[1]),rep("ffrd", times=dim(sherry)[1]))
  sherry4$year<-2003
  sherry4$site<-"oklahoma"
  sherryok<-subset(sherry4, select=c("site","plot","event","year","genus","species", "doy"))
  #sherryok$variety <- NA
  #sherryok$cult <- NA
  #file2 <- file.path(path, "SherryPhenology2003_First6spp.csv")#need to add these in- they're just a little different than others in formattin
  #sherryotherspp<-read.csv(file2, header=T)
  sherryok<-sherryok[!is.na(sherryok$doy),]
  
  return(sherryok)
}

##Price & Wasser data RMBL
## Data type: FFd, FFRD, SD
## Notes: mary.price@ucr.edu
#From Mary: "We chose up to 5 flowering individuals that spanned the elevational range of flowering individuals of that species on the small moraine.  The "DIST" column indicates the meters downslope from the upper edge of the plot.
#There is some inconsistency in the ordering of columns.  Sometimes the "species" column domes before the "individual ID" column, and sometimes not -- you'll have to look for that if you concatenate files.
#The "comments" column after each census date column sometimes includes estimates of fruiting success, measured as #fruits/#flowers on each plant, along with notes on whether the plant got frosted, parasitized, replaced, and the like.  I can help you translate the notes if you have problems and need to know what the notes mean.  The plant species vary considerably, as you know, in what fruit set means.  Our notes are incomplete (at least, I haven't found the relevant info in spot-checks of notes--since we didn't include fruit set info in the analyses for our published paper, I'd have to search...), but I suspect that for Asteraceae we used the head as the "flower" unit, and probably we used the umbel as the unit for Polygonaceae.
#In all cases, 0 = not flowering; 1 = bud present; 2 = open flower present; 3   = senescent flower present (corolla still attached); 4 = initiated fruit (corolla off); 5 = expanding fruit; 6 = dehisced fruit.
#Of course, these stages mean different things for each species.
#Species names have changed in some cases, or if one uses different authorities.  For example, Potentilla gracilis is now P. pulcherrima, and I think Bill Weber is the only one who advocates the genus "Seriphidium" for shrubby Artemisia.  So you may want to double-check names.
#The number of files should correspond more-or-less with the number of years reported in the 1998 paper, with the addition of a few files from 1990 censuses (which can probably be left out of analyses since we were working the bugs out of our methods in that year).
clean.raw$price <- function(filename, path) {
 pricefiles<-c("ATPHEN90g.csv","ATPHEN91g.csv","ATPHEN92g.csv","ATPHEN94g.csv","CLPHEN91g.csv","CLPHEN92g.csv","CLPHEN93g.csv","CLPHEN94g.csv","CRPHEN92g.csv","CRPHEN93g.csv","CRPHEN94g.csv","DNPHEN90g.csv","DNPHEN91g.csv","DNPHEN92g.csv","DNPHEN93g.csv","DNPHEN94g.csv","EGPHEN90g.csv","EGPHEN91g.csv","EGPHEN92g.csv","EGPHEN93g.csv","EGPHEN94g.csv","ESPHEN90g.csv","ESPHEN91g.csv","ESPHEN92g.csv","ESPHEN93g.csv","ESPHEN94g.csv","IAPHEN90g.csv","IAPHEN91g.csv","IAPHEN92g.csv","IAPHEN93g.csv","IAPHEN94g.csv","LLPHEN90G.csv","LLPHEN91G.csv","LLPHEN92G.csv","LLPHEN93G.csv","LLPHEN94G.csv","MFPHEN90G.csv","MFPHEN91g.csv","MFPHEN92g.csv","MFPHEN93g.csv","MFPHEN94g.csv","PGPHEN91g.csv","PGPHEN92g.csv","PGPHEN93g.csv","PGPHEN94g.csv")
  skiplines<-c(3,3,2,2,rep(3,times=41))  
  price <- NA
  for (i in 1:length(pricefiles)){
    file <- file.path(path, paste(pricefiles[i]))
    price1 <- read.csv(file, skip=skiplines[i], header=TRUE)
    colnames(price1)[which(colnames(price1)=="PLOT"|colnames(price1)=="plot")]<-"plot"
    colnames(price1)[which(colnames(price1)=="SPE")]<-"SP"
    price1<-price1[!is.na(price1$SP),]
    price1<-price1[!is.na(price1$plot),]
    #estimate first date of open flowers, fruits, and seeds dispersing
    firstsurv<-min(which(substr(colnames(price1),1,1)=="X"))
    lastsurv<-max(which(substr(colnames(price1),1,1)=="X"))    
    get.ffd <- function(x) names(x)[which(x==2|x==12|x==123|x==1234|x==12345|x==123456)][1]#first flower date
    get.ffrd <- function(x) names(x)[which(x==4|x==34|x==234|x==1234|x==12345|x==123456)][1]#first fruit date
    get.sd <- function(x) names(x)[which(x==6|x==56|x==456|x==3456|x==2345|x==123456)][1]#seed dispersal/fruit dehiscing date
    
    ffd<-substr(apply(price1[,firstsurv:lastsurv],1,get.ffd),2,9)
    ffrd<-substr(apply(price1[,firstsurv:lastsurv],1,get.ffrd),2,9)
    sd<-substr(apply(price1[,firstsurv:lastsurv],1,get.sd),2,9)
    price2<-cbind(price1,ffd,ffrd,sd)  
    price2$filename<-paste(pricefiles[i])
    price2$year<-paste("19",substr(price2$filename,7,8),sep="")
    price3<-subset(price2, select=c("plot","SP", "ffd","ffrd","sd","filename","year"))
    price<-rbind(price,price3)
  }
 price<-price[-1,]
 price4<-reshape(price,varying = list(names(price)[3:5]), direction = "long", v.names = c("date"), times = c(names(price)[3:5]))
 colnames(price4)[5]<-"event"
 price4$site<-"rmbl"
 price4$date[which(is.na(price4$date))]<-"NA.NA.NA"
 price4$doy<-strftime(strptime(price4$date, format = "%m.%d.%y"),format = "%j") 
 price4$genus<-NA
 price4$species<-NA
 price4$genus[price4$SP=="AT"] <- "Artemesia"
 price4$species[price4$SP=="AT"] <- "tridentata"
 price4$genus[price4$SP=="CL"] <- "Claytonia"
 price4$species[price4$SP=="CL"] <- "lanceolata"
 price4$genus[price4$SP=="CR"] <- "Campanula"
 price4$species[price4$SP=="CR"] <- "rotundifolia"
 price4$genus[price4$SP=="DN"] <- "Delphinium"
 price4$species[price4$SP=="DN"] <- "nelsonii"
 price4$genus[price4$SP=="EG"] <- "Erythronium"
 price4$species[price4$SP=="EG"] <- "grandiflorum"
 price4$genus[price4$SP=="ES"] <- "Eriogonum"
 price4$species[price4$SP=="ES"] <- "subalpinum"
 price4$genus[price4$SP=="es"] <- "Eriogonum"
 price4$species[price4$SP=="es"] <- "subalpinum"
 price4$genus[price4$SP=="IA"] <- "Ipomopsis"
 price4$species[price4$SP=="IA"] <- "aggregata"
 price4$genus[price4$SP=="ia"] <- "Ipomopsis"
 price4$species[price4$SP=="ia"] <- "aggregata"
 price4$genus[price4$SP=="LL"] <- "Lathyrus"
 price4$species[price4$SP=="LL"] <- "leucanthus"
 price4$genus[price4$SP=="MF"] <- "Mertensia"
 price4$species[price4$SP=="MF"] <- "fusiformes"
 price4$genus[price4$SP=="PG"] <- "Potentilla"
 price4$species[price4$SP=="PG"] <- "gracilis"
 price4<-price4[!is.na(price4$doy),]
 price5<-subset(price4, select=c("site","plot","event","year","genus","species", "doy"))
 #price5$variety <- NA
 #price5$cult <- NA
  return(price5)
}

##Data from Chuine
##no plots listed for 2003
clean.raw$chuine <- function(filename, path="./Experiments/chuine") {
  chuinefiles<-c("Chuine_pheno_2002.csv","Chuine_pheno_2003_cleaned.csv","Chuine_pheno_2004.csv","Chuine_pheno_2005.csv")
  years<-c(2002,2003,2004,2005)
  chuine <- NA
  for (i in 1:length(chuinefiles)){
    file <- file.path(path, paste(chuinefiles[i]))
    chuine1 <- read.csv(file, header=TRUE)
    chuine1$plot<-paste(chuine1$block,chuine1$Plot,sep="")
    colnames(chuine1)[which(colnames(chuine1)=="species"|colnames(chuine1)=="Species")]<-"sp"
    colnames(chuine1)[which(colnames(chuine1)=="X55")]<-"ffb"
    colnames(chuine1)[which(colnames(chuine1)=="X65")]<-"ffd"
    colnames(chuine1)[which(colnames(chuine1)=="X85")]<-"ffrd"
    colnames(chuine1)[which(colnames(chuine1)=="X91")]<-"91"
    #colnames(chuine1)[which(colnames(chuine1)=="X95")]<-"sen"
    chuine1<-chuine1[!is.na(chuine1$sp),]
    phen1<-which(colnames(chuine1)=="ffb")
    phen2<-which(colnames(chuine1)=="ffrd")
    chuine2<-reshape(chuine1,varying = list(names(chuine1)[phen1:phen2]), direction = "long", v.names = c("date"), times = c(names(chuine1)[phen1:phen2]))
    chuine2$year<-paste(years[i])
    chuine2<-chuine2[!chuine2$date=="",]
    colnames(chuine2)[which(colnames(chuine2)=="time")]<-"event"
    if(years[i]==2002){chuine2$doy<-strftime(strptime(chuine2$date, format = "%d/%m/%y"),format = "%j")}
    if(years[i]==2003){chuine2$doy<-strftime(strptime(chuine2$date, format = "%d-%b"),format = "%j")}
    if(years[i]==2004){chuine2$doy<-strftime(strptime(chuine2$date, format = "%d-%b"),format = "%j")}
    if(years[i]==2005){chuine2$doy<-strftime(strptime(chuine2$date, format = "%d-%b"),format = "%j")}
    
    chuine3<-subset(chuine2, select=c("plot","sp", "event","year","doy"))
    chuine<-rbind(chuine,chuine3)
  }
  chuine<-chuine[-1,]
  chuine$genus<-NA
  chuine$species<-NA
  chuine$genus[chuine$sp=="aa"] <- "Artemesia"
  chuine$species[chuine$sp=="aa"] <- "annua"
  chuine$genus[chuine$sp=="av"] <- "Artemesia"
  chuine$species[chuine$sp=="av"] <- "vulgaris"
  chuine$genus[chuine$sp=="ar"] <- "Amaranthus"
  chuine$species[chuine$sp=="ar"] <- "retroflexus"
  chuine$genus[chuine$sp=="ad"] <- "Amaranthus"
  chuine$species[chuine$sp=="ad"] <- "deflexus"
  chuine$genus[chuine$sp=="qr"] <- "Quercus"
  chuine$species[chuine$sp=="qr"] <- "robur"
  chuine$genus[chuine$sp=="qp"] <- "Quercus"
  chuine$species[chuine$sp=="qp"] <- "pubescens"
  chuine$genus[chuine$sp=="qi"] <- "Quercus"
  chuine$species[chuine$sp=="qi"] <- "ilex"
  chuine$genus[chuine$sp=="lr"] <- "Lolium"
  chuine$species[chuine$sp=="lr"] <- "rigidum"
  chuine$genus[chuine$sp=="lp"] <- "Lolium"
  chuine$species[chuine$sp=="lp"] <- "perenne"
  chuine$genus[chuine$sp=="sv"] <- "Setaria"
  chuine$species[chuine$sp=="sv"] <- "viridis"
  chuine$genus[chuine$sp=="sp"] <- "Setaria"
  chuine$species[chuine$sp=="sp"] <- "parviflora"
  chuine$site<-"chuine"
  chuine4<-subset(chuine, select=c("site","plot","event","year","genus","species", "doy"))
  #chuine4$variety <- NA
  #chuine4$cult <- NA
  chuine4<-chuine4[!is.na(chuine4$doy),]
  return(chuine4)
  ##
}
# Produce cleaned raw data
#
raw.data.dir <- "./Experiments"
cleandata.raw <- list()
cleandata.raw$marchin <- clean.raw$marchin(path="./Experiments/marchin")
cleandata.raw$bace <- clean.raw$bace(path="./Experiments/bace")
cleandata.raw$farnsworth <- clean.raw$farnsworth(path="./Experiments/farnsworth")
cleandata.raw$jasper <- clean.raw$jasper(path="./Experiments/jasper")
cleandata.raw$clarkduke <- clean.raw$clarkduke(path="./Experiments/clark")
cleandata.raw$clarkharvard <- clean.raw$clarkharvard(path="./Experiments/clark")
cleandata.raw$sherry <- clean.raw$sherry(path="./Experiments/sherry")
cleandata.raw$price <- clean.raw$price(path="./Experiments/price")
cleandata.raw$chuine<- clean.raw$chuine(path="./Experiments/chuine")
expphendb <- do.call("rbind", cleandata.raw)
row.names(expphendb) <- NULL
#expphendb<-expphendb[!is.na(expphendb$doy),]
write.csv(expphendb, "radmeeting/exppheno.csv", row.names=FALSE)
dim(expphendb)
#64112 rows, from 
head(expphendb)
unique(expphendb$site)#9 experiments (will be 10 with Aaron ellisons phenology, 11 with zackenberg) across 7 sites (8 with zackenberg)
unique(expphendb$plot)#some NA/blanks to fix...
unique(expphendb$genus)#59 genera
expphendb$genus.species<-paste(expphendb$genus,expphendb$species,sep=".")
unique(expphendb$genus.species)#97 species
unique(expphendb$event)#7 phenological events
tapply(expphendb$doy,list(expphendb$site,expphendb$event),length)
max(as.numeric(expphendb$doy))
