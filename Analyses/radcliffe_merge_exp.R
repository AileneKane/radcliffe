### Started 8 December 2015 ##
### By Ailene Ettinger ###
setwd("~/Documents/GitHub/radcliffe")
rm(list=ls()) 
options(stringsAsFactors=FALSE)
##load packages
library(reshape)
library(tidyr)
# make list to store all the derived dataset cleaning functions
clean.raw <- list()
clean.raw$marchin <- function(filename="Budburst_Marchin.csv", path="./Data/Experiments/marchin") {
  
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
  marchin1a$site <- "exp10"
  marchin1a$event <- "bbd"
  marchin2<-read.csv("Data/Experiments/marchin/Flower_Marchin.csv", header=T)
  names(marchin2)[2] <- "genusspecies"
  names(marchin2)[1] <- "year"
  names(marchin2)[3] <- "plot"
  names(marchin2)[7] <- "doy"  
  marchin2a<- subset(marchin2, select=c("year","genusspecies","plot", "doy"))
  marchin2a$site <- "exp10"
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
  marchin3$block<-NA
  marchin<-subset(marchin3, select=c("site","block","plot","event","year","genus","species", "doy"))
  #marchin$variety <- NA
  #marchin$cult <- NA
  return(marchin)
}

clean.raw$bace <- function(filename="BACE_deciduous2010_originaltrees.csv", path="./Data/Experiments/bace") {  
  ##BACE ##
  ## Data type: BBD,LOD,LUD ##
  ## Notes: Jeff Dukes = contact, years 2010,2011,2013
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
  bace1a$site <- "exp01"
  names(bace1a)[3]<-"doy"
  bace1b<- subset(bace1, select=c("genusspecies","plot", "doy_lo"))
  bace1b$event <- "lod"
  bace1b$year <- 2010
  bace1b$site <- "exp01"
  names(bace1b)[3]<-"doy"
  bace1c<- subset(bace1, select=c("genusspecies","plot", "doy_lunf"))
  bace1c$event <- "lud"
  bace1c$year <- 2010
  bace1c$site <- "exp01"
  names(bace1c)[3]<-"doy"
  file2 <- file.path(path, "BACE_pinustrobus2010_originaltrees.csv")
  bace2 <- read.csv(file2, check.names=FALSE, header=TRUE,na.strings = ".")
  names(bace2)[5] <- "genusspecies"
  names(bace2)[1] <- "plot"
  names(bace2)[6] <- "doy_bbd"
  names(bace2)[8] <- "doy_fgn"
  names(bace2)[10] <- "doy_fnb"
  names(bace2)[12] <- "doy_fyn"
  bace2a<-subset(bace2, select=c("genusspecies","plot", "doy_bbd"))
  bace2a$event <- "bbd"
  bace2a$year <- 2010
  bace2a$site <- "exp01"
  names(bace2a)[3]<-"doy"
  bace2b<- subset(bace2, select=c("genusspecies","plot", "doy_fgn"))
  bace2b$event <- "fgn"
  bace2b$year <- 2010
  bace2b$site <- "exp01"
  names(bace2b)[3]<-"doy"
  bace2c<- subset(bace2, select=c("genusspecies","plot", "doy_fnb"))
  bace2c$event <- "fnb"
  bace2c$year <- 2010
  bace2c$site <- "exp01"
  names(bace2c)[3]<-"doy"
  bace2c<- subset(bace2, select=c("genusspecies","plot", "doy_fnb"))
  bace2c$event <- "fnb"
  bace2c$year <- 2010
  bace2c$site <- "exp01"
  names(bace2c)[3]<-"doy"
  bace2d<- subset(bace2, select=c("genusspecies","plot", "doy_fyn"))
  bace2d$event <- "fyn"
  bace2d$year <- 2010
  bace2d$site <- "exp01"
  names(bace2d)[3]<-"doy"
  #2011
  file3 <- file.path(path, "2011BACEherbaceousphenologydata11_11CEG.csv")
  bace3 <- read.csv(file3, skip=1, header=TRUE)
  bace3<-bace3[1:min(which(bace3$Plot=="")),]
  names(bace3)[2] <- "genusspecies"
  names(bace3)[1] <- "plot"
  bace3$doy_ffd<-bace3[,6]
  bace3[which(bace3$doy_ffd=="" & bace3$open.flowers>0),]$doy_ffd<-bace3[which(bace3$doy_ffd=="" & bace3$open.flowers>0),]$open.flowers
  bace3a<- subset(bace3, select=c("genusspecies","plot", "doy_ffd"))
  bace3a$event <- "ffd"
  bace3a$year <- 2011
  bace3a$site <- "exp01"
  names(bace3a)[3]<-"doy"
  #2012
  file4 <- file.path(path, "2012_BACESpringTreePhenology_04_04_2013_CEG_EW.csv")
  bace4 <- read.csv(file4, skip=1, header=TRUE,na.strings=".")
  bace4<-bace4[1:468,]#remove blank rows at bottom
  names(bace4)[5] <- "genusspecies"
  names(bace4)[1] <- "plot"
  names(bace4)[6] <- "doy_bbd"#use first bbd (not >3)
  names(bace4)[8] <- "doy_lud"#leaf unfolding date
  names(bace4)[10] <- "doy_lod"#leaf out date
  bace4a<- subset(bace4, select=c("genusspecies","plot", "doy_bbd"))
  bace4a$event <- "bbd"
  bace4a$year <- 2012
  bace4a$site <- "exp01"
  names(bace4a)[3]<-"doy"
  bace4b<- subset(bace4, select=c("genusspecies","plot", "doy_lud"))
  bace4b$event <- "lud"
  bace4b$year <- 2012
  bace4b$site <- "exp01"
  names(bace4b)[3]<-"doy"
  bace4c<- subset(bace4, select=c("genusspecies","plot", "doy_lod"))
  bace4c$event <- "lod"
  bace4c$year <- 2012
  bace4c$site <- "exp01"
  names(bace4c)[3]<-"doy"
  file5 <- file.path(path, "2012_BACESpringTreePhenology_04_04_2013_CEG_NS.csv")
  bace5 <- read.csv(file5, skip=1, header=TRUE,na.strings=".")
  bace5<-bace5[1:624,]#remove blank rows ar bottom, if there are any
  names(bace5)[5] <- "genusspecies"
  names(bace5)[1] <- "plot"
  names(bace5)[6] <- "doy_bbd"#use first bbd (not >3)
  names(bace5)[8] <- "doy_lud"#leaf unfolding date
  names(bace5)[10] <- "doy_lod"#leaf out date
  bace5a<- subset(bace5, select=c("genusspecies","plot", "doy_bbd"))
  bace5a$event <- "bbd"
  bace5a$year <- 2012
  bace5a$site <- "exp01"
  names(bace5a)[3]<-"doy"
  bace5b<- subset(bace5, select=c("genusspecies","plot", "doy_lud"))
  bace5b$event <- "lud"
  bace5b$year <- 2012
  bace5b$site <- "exp01"
  names(bace5b)[3]<-"doy"
  bace5c<- subset(bace5, select=c("genusspecies","plot", "doy_lod"))
  bace5c$event <- "lod"
  bace5c$year <- 2012
  bace5c$site <- "exp01"
  names(bace5c)[3]<-"doy"
  #file6 <- file.path(path, "2012_BACESpringTreePhenology_04_04_2013_CEG_Outer.csv")#leave these out since they were just outside the main treatment area, were caged, and did not get all treatments
  #bace6 <- read.csv(file6, skip=1, header=TRUE,na.strings=".")
  #bace6<-bace6[1:576,]#remove blank rows at bottom
  #names(bace6)[4] <- "genusspecies"
  #names(bace6)[1] <- "plot"
  #names(bace6)[5] <- "doy_bbd"#use first bbd (not >3)
  #names(bace6)[7] <- "doy_lud"#leaf unfolding date
  #names(bace6)[9] <- "doy_lod"#leaf out date
  #bace6a<- subset(bace6, select=c("genusspecies","plot", "doy_bbd"))
  #bace6a$event <- "bbd"
  #bace6a$year <- 2012
  #bace6a$site <- "exp01"
  #names(bace6a)[3]<-"doy"
  #bace6b<- subset(bace6, select=c("genusspecies","plot", "doy_lud"))
  #bace6b$event <- "lud"
  #bace6b$year <- 2012
  #bace6b$site <- "exp01"
  #names(bace6b)[3]<-"doy"
  #bace6c<- subset(bace6, select=c("genusspecies","plot", "doy_lod"))
  #bace6c$event <- "lod"
  #bace6c$year <- 2012
  #bace6c$site <- "exp01"
  #names(bace6c)[3]<-"doy"
  file7 <- file.path(path, "2012_BACESpringTreePhenology_04_04_2013_CEG_pinus.csv")
  bace7 <- read.csv(file7, skip=1, header=TRUE,na.strings=".")
  bace7<-bace7[1:156,]#remove blank rows at bottom
  names(bace7)[5] <- "genusspecies"
  names(bace7)[1] <- "plot"
  names(bace7)[6] <- "doy_bbd"#first bud bolted
  names(bace7)[8] <- "doy_fgn"#first green needles
  names(bace7)[10] <- "doy_fnb"#first needle bundles
  names(bace7)[12] <- "doy_fyn"#first young needles
  bace7a<- subset(bace7, select=c("genusspecies","plot", "doy_bbd"))
  bace7a$event <- "bbd"
  bace7a$year <- 2012
  bace7a$site <- "exp01"
  names(bace7a)[3]<-"doy"
  bace7b<- subset(bace7, select=c("genusspecies","plot", "doy_fgn"))
  bace7b$event <- "fgn"
  bace7b$year <- 2012
  bace7b$site <- "exp01"
  names(bace7b)[3]<-"doy"
  bace7c<- subset(bace7, select=c("genusspecies","plot", "doy_fnb"))
  bace7c$event <- "fnb"
  bace7c$year <- 2012
  bace7c$site <- "exp01"
  names(bace7c)[3]<-"doy"
  bace7d<- subset(bace7, select=c("genusspecies","plot", "doy_fyn"))
  bace7d$event <- "fyn"
  bace7d$year <- 2012
  bace7d$site <- "exp01"
  names(bace7d)[3]<-"doy"
  #2009
  file8 <- file.path(path, "2009BACEdeciduoustreespringphenology.csv")
  bace8 <- read.csv(file8, header=TRUE)
  names(bace8)[9] <- "genusspecies"
  names(bace8)[4] <- "plot"
  names(bace8)[17] <- "doy_bbd"#use first bbd (not >3)
  names(bace8)[19] <- "doy_lud"#leaf unfolding date
  names(bace8)[21] <- "doy_lod"#leaf out date
  bace8a<- subset(bace8, select=c("genusspecies","plot", "doy_bbd"))
  bace8a$event <- "bbd"
  bace8a$year <- 2012
  bace8a$site <- "exp01"
  names(bace8a)[3]<-"doy"
  bace8b<- subset(bace8, select=c("genusspecies","plot", "doy_lud"))
  bace8b$event <- "lud"
  bace8b$year <- 2012
  bace8b$site <- "exp01"
  names(bace8b)[3]<-"doy"
  bace8c<- subset(bace8, select=c("genusspecies","plot", "doy_lod"))
  bace8c$event <- "lod"
  bace8c$year <- 2012
  bace8c$site <- "exp01"
  names(bace8c)[3]<-"doy"
  
  #2013
  file9 <- file.path(path, "2013BACEherbaceousphenologydatasheet.csv")
  bace9 <- read.csv(file9, skip=1, header=TRUE)
  bace9<-bace9[1:min(which(bace9$Plot=="")),]
  names(bace9)[2] <- "genusspecies"
  names(bace9)[1] <- "plot"
  bace9$doy_ffd<-bace9[,6]
  bace9[which(bace9$doy_ffd=="" & bace9$open.flowers>0),]$doy_ffd<-bace9[which(bace9$doy_ffd=="" & bace9$open.flowers>0),]$open.flowers
  bace9a<- subset(bace9, select=c("genusspecies","plot", "doy_ffd"))
  bace9a$event <- "ffd"
  bace9a$year <- 2013
  bace9a$site <- "exp01"
  names(bace9a)[3]<-"doy"
  #put them all together
  baceall<-rbind(bace1a,bace1b,bace1c,bace2a,bace2b,bace2c,bace2d,bace3a,bace4a,bace4b,bace4c,bace5a,bace5b,bace5c,bace7a,bace7b,bace7c,bace7d,bace8a,bace8b,bace8c,bace9a)
  baceall<-baceall[-which(baceall$genusspecies==""),]
  baceall<-baceall[-which(baceall$genusspecies=="Genus sp."),]
  baceall<-baceall[-which(baceall$genusspecies=="moss"),]
  baceall<-baceall[-which(baceall$genusspecies=="Oregano"),]
  baceall[baceall$genusspecies=="Giant fox tail",]$genusspecies<-"Setaria faberi"
  baceall[baceall$genusspecies=="Setarir glauca",]$genusspecies<-"Setaria glauca"
  baceall[baceall$genusspecies=="Setaria viridens",]$genusspecies<-"Setaria viridis"
  baceall[baceall$genusspecies=="conyza canadensis",]$genusspecies<-"Conyza canadensis"
  baceall[baceall$genusspecies=="linnaria vulgaris"|baceall$genusspecies=="Linneria vulgaris",]$genusspecies<-"Linaria vulgaris"
  baceall[baceall$genusspecies=="A. rubrum "|baceall$genusspecies=="A. rubrum  "|baceall$genusspecies=="A. rubrum"|baceall$genusspecies=="A. rubrum (main stem)",]$genusspecies <- "Acer rubrum"
  baceall[baceall$genusspecies=="B. lenta  "|baceall$genusspecies=="B. lenta   "|baceall$genusspecies=="B. lenta"|baceall$genusspecies=="B. lenta (main stem)",]$genusspecies <- "Betula lenta"
  baceall[baceall$genusspecies=="Q. rubra  "|baceall$genusspecies=="Q. rubra"|baceall$genusspecies=="Q. rubra   ",]$genusspecies <- "Quercus rubra"
  baceall[baceall$genusspecies=="P. strobus"|baceall$genusspecies=="P. strobus ",]$genusspecies <- "Pinus strobus"
  baceall[baceall$genusspecies=="B. popul" ,]$genusspecies<- "Betula populifolia"
  baceall[baceall$genusspecies=="U. americana" ,]$genusspecies<- "Ulmus americana"
  baceall[baceall$genusspecies=="P. grand" ,]$genusspecies<- "Populus grandidentata"
  baceall[baceall$genusspecies=="P. serotina" ,]$genusspecies<- "Prunus serotina"
  baceall[baceall$genusspecies=="Polygonum" ,]$genusspecies<- "Polygonum sp."
  baceall[baceall$genusspecies=="Asclepias syriaca ",]$genusspecies<-"Asclepias syriaca"
  baceall[baceall$genusspecies=="Capsella bursa-pastoris ",]$genusspecies<-"Capsella bursa-pastoris"
  baceall[baceall$genusspecies=="Cerastium fontanum ",]$genusspecies<-"Cerastium fontanum"
  baceall[baceall$genusspecies=="Dactylis glomerata ",]$genusspecies<-"Dactylis glomerata"
  baceall[baceall$genusspecies=="Draba verna ",]$genusspecies<-"Draba verna"
  baceall[baceall$genusspecies=="Elymus repens ",]$genusspecies<-"Elymus repens"
  baceall[baceall$genusspecies=="Erigeron annuus ",]$genusspecies<-"Erigeron annuus"
  baceall[baceall$genusspecies=="Festuca spp. ",]$genusspecies<-"Festuca sp."
  baceall[baceall$genusspecies=="lepidium virginicum",]$genusspecies<-"Lepidium virginicum"
  baceall[baceall$genusspecies=="Lamium amplexicaula",]$genusspecies<-"Lamium amplexicaule"
  baceall[baceall$genusspecies=="Oxalis stricta ",]$genusspecies<-"Oxalis stricta"
  baceall[baceall$genusspecies=="Rumex crispus ",]$genusspecies<-"Rumex crispus"
  baceall[baceall$genusspecies=="Silene alba ",]$genusspecies<-"Silene alba"
  baceall[baceall$genusspecies=="Tanacetum vulgare ",]$genusspecies<-"Tanacetum vulgare"
  baceall[baceall$genusspecies=="Taraxacum officinale ",]$genusspecies<-"Taraxacum officinale"
  baceall[baceall$genusspecies=="Trifolium pratense ",]$genusspecies<-"Trifolium pratense"
  baceall[baceall$genusspecies=="Trifolium repens ",]$genusspecies<-"Trifolium repens"
  baceall[baceall$genusspecies=="Veronica arvensis ",]$genusspecies<-"Veronica arvensis"
  baceall[baceall$genusspecies=="Phleum pratense " ,]$genusspecies<-"Phleum pratense" 
  baceall[baceall$genusspecies=="Plantago lanceolata ",]$genusspecies<-"Plantago lanceolata"
  baceall[baceall$genusspecies=="Potentilla argentea ",]$genusspecies<-"Potentilla argentea"
  
  baceALL<-baceall %>% separate(genusspecies, c("genus", "species"), sep=" ", remove=F)
  baceALL$block<-NA
  #baceALL[baceALL$plot=="C1"|baceALL$plot=="C2"|baceALL$plot=="C3",]$block<-NA
  baceALL[which(as.numeric(baceALL$plot)<13),]$block<-1
  baceALL[which(as.numeric(baceALL$plot)<25 & as.numeric(baceALL$plot)>12),]$block<-2
  baceALL[which(as.numeric(baceALL$plot)<37 & as.numeric(baceALL$plot)>24),]$block<-3
  baceALL[which(as.numeric(baceALL$plot)>36),]$block<-0# for some reason there are a few plots  less than 36 that show up as block= 0. fix this
  bace<-subset(baceALL, select=c("site","block","plot","event","year","genus","species", "doy"))
  #bace$variety <- NA
  #bace$cult <- NA
  bace<-bace[!is.na(bace$doy),]
  bace<-bace[-which(bace$doy==""),]
  bace<-bace[-which(substr(bace$doy,1,1)=="<"),]
  bace<-bace[-which(bace$plot=="C1"|bace$plot=="C2"|bace$plot=="C3"|bace$plot=="40"|bace$plot=="41"|bace$plot=="42"),]#outside treatment area
  
  return(bace)
}

##Farnsworth from Harvard ##
## Data type: BBD,LOD,LUD,FFD ##
## Notes: Contact: Public data, http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf033 ##
##Question: hf033-01-diameter-1.csv files says plot 17= intact control (Treat=1) but soil temp file says plot17=disturbance control (=d)
clean.raw$farnsworth <- function(filename="hf033-01-diameter-1.csv", path="./Data/Experiments/farnsworth/",names.only=FALSE) {
  file <- file.path(path, filename)
  farnsworth1 <- read.csv(file, check.names=FALSE, header=TRUE)
  #phenological stage 1.5=budburst; need to get day of year for which this occurred
  colnames(farnsworth1)[3]<-"plot"
  farnsworth1$bb_doy<-NA
  for(i in 1:dim(farnsworth1)[1]){
    inddat<-farnsworth1[i,20:31]
    names(inddat)[1:12]<-c("1993-04-16","1993-04-23","1993-05-2","1993-05-17","1993-05-24" ,"1993-06-07","1993-07-09","1993-07-23","1993-08-11","1993-09-09","1993-09-25","1993-10-23")
    bbdate<-names(inddat)[min(which(inddat==1.5))]#1.5="leaves just emerging"
    bbdoy<-strftime(bbdate, format = "%j")
    farnsworth1$bb_doy[i]<-bbdoy
  }
  #now phenological stage 2=leaves expanding; need to get day of year for which this occurred
  farnsworth1$leafunf_doy<-NA
  for(i in 1:dim(farnsworth1)[1]){
    inddat<-farnsworth1[i,20:31]
    names(inddat)[1:12]<-c("1993-04-16","1993-04-23","1993-05-2","1993-05-17","1993-05-24" ,"1993-06-07","1993-07-09","1993-07-23","1993-08-11","1993-09-09","1993-09-25","1993-10-23")
    ludate<-names(inddat)[min(which(inddat==2))]
    ludoy<-strftime(ludate, format = "%j")
    farnsworth1$leafunf_doy[i]<-ludoy
  }
  #now phenological stage 3=leaves fully expanded=leafout; need to get day of year for which this occurred
  farnsworth1$leafout_doy<-NA
  for(i in 1:dim(farnsworth1)[1]){
    inddat<-farnsworth1[i,20:31]
    names(inddat)[1:12]<-c("1993-04-16","1993-04-23","1993-05-2","1993-05-17","1993-05-24" ,"1993-06-07","1993-07-09","1993-07-23","1993-08-11","1993-09-09","1993-09-25","1993-10-23")
    lodate<-names(inddat)[min(which(inddat==3))]
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
  #now leaf coloration=leaves turned color" first date this was observed
  farnsworth1$col<-NA
  for(i in 1:dim(farnsworth1)[1]){
    inddat<-farnsworth1[i,20:31]
    names(inddat)[1:12]<-c("1993-04-16","1993-04-23","1993-05-2","1993-05-17","1993-05-24" ,"1993-06-07","1993-07-09","1993-07-23","1993-08-11","1993-09-09","1993-09-25","1993-10-23")
    coldate<-names(inddat)[min(which(inddat==7))]
    coldoy<-strftime(coldate, format = "%j")
    farnsworth1$col[i]<-coldoy
  }
  #now leaf drop="some or all leaves abscised" (8,9)-first date this was observed
  farnsworth1$drop<-NA
  for(i in 1:dim(farnsworth1)[1]){
    inddat<-farnsworth1[i,20:31]
    names(inddat)[1:12]<-c("1993-04-16","1993-04-23","1993-05-2","1993-05-17","1993-05-24" ,"1993-06-07","1993-07-09","1993-07-23","1993-08-11","1993-09-09","1993-09-25","1993-10-23")
    dropdate<-names(inddat)[min(which(inddat>7))]
    dropdoy<-strftime(dropdate, format = "%j")
    farnsworth1$drop[i]<-dropdoy
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
  farnsworth1$site<-"exp08"  
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
  farnsworth3a<-subset(farnsworth3, select=c("site","plot","event","year","genus","species1","leafunf_doy"))
  colnames(farnsworth3a)[6]<-"species"
  colnames(farnsworth3a)[7]<-"doy"
  farnsworth3a$event <- "lud"
  #pull out all leafout rows
  farnsworth4<-farnsworth1[which(farnsworth1$leafout_doy>0),]
  farnsworth4a<-subset(farnsworth4, select=c("site","plot","event","year","genus","species1","leafout_doy"))
  colnames(farnsworth4a)[6]<-"species"
  colnames(farnsworth4a)[7]<-"doy"
  farnsworth4a$event <- "lod"
  #pull out all flowering rows
  farnsworth5<-farnsworth1[which(farnsworth1$ffd>0),]
  farnsworth5a<-subset(farnsworth5, select=c("site","plot","event","year","genus","species1","ffd"))
  colnames(farnsworth5a)[6]<-"species"
  colnames(farnsworth5a)[7]<-"doy"
  farnsworth5a$event <- "ffd"
  #pull out all fruiting rows
  farnsworth6<-farnsworth1[which(farnsworth1$ffrd>0),]
  farnsworth6a<-subset(farnsworth6, select=c("site","plot","event","year","genus","species1","ffrd"))
  colnames(farnsworth6a)[6]<-"species"
  colnames(farnsworth6a)[7]<-"doy"
  farnsworth6a$event <- "ffrd"
  #pull out all coloration rows
  farnsworth7<-farnsworth1[which(farnsworth1$col>0),]
  farnsworth7a<-subset(farnsworth7, select=c("site","plot","event","year","genus","species1","col"))
  colnames(farnsworth7a)[6]<-"species"
  colnames(farnsworth7a)[7]<-"doy"
  farnsworth7a$event <- "col"
  #pull out all drop rows
  farnsworth8<-farnsworth1[which(farnsworth1$drop>0),]
  
  farnsworth8a<-subset(farnsworth7, select=c("site","plot","event","year","genus","species1","drop"))
  colnames(farnsworth8a)[6]<-"species"
  colnames(farnsworth8a)[7]<-"doy"
  farnsworth8a$event <- "drop"
  
  alldat<- rbind(farnsworth2a,farnsworth3a,farnsworth3a,farnsworth4a,farnsworth5a,farnsworth6a,farnsworth7a,farnsworth8a)
  alldat$block<-NA
  alldat[alldat$plot<4,]$block<-1
  alldat[alldat$plot==4|alldat$plot==5|alldat$plot==6,]$block=2
  alldat[alldat$plot==7|alldat$plot==8|alldat$plot==9,]$block=3
  alldat[alldat$plot==10|alldat$plot==11|alldat$plot==12,]$block=4
  alldat[alldat$plot==13|alldat$plot==14|alldat$plot==15,]$block=5
  alldat[alldat$plot==16|alldat$plot==17|alldat$plot==18,]$block=6
  farnsworth<-subset(alldat, select=c("site","block","plot","event","year","genus","species","doy"))
  return(farnsworth)
}
###Cleland et al Jasper Ridge data
###FFD
clean.raw$cleland <- function(filename="JasperRidge_data.csv", path="./Data/Experiments/cleland") {
  file <- file.path(path, filename)
  cleland1 <- read.csv(file, check.names=FALSE, header=TRUE)  
  cleland1<-cleland1[cleland1$CO2==1,]#remove plots with CO2 added
  cleland1<-cleland1[cleland1$Nutrient==1,]#remove plots with N added
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
  cleland1$site<-"exp05"
  cleland1$event<-"ffd"
  cleland1$block<-NA
  cleland1[cleland1$plot==1|cleland1$plot==2|cleland1$plot==3|cleland1$plot==33|cleland1$plot==4,]$block<-1
  cleland1[cleland1$plot==12|cleland1$plot==7|cleland1$plot==8|cleland1$plot==9,]$block<-2
  cleland1[cleland1$plot==10|cleland1$plot==11|cleland1$plot==5|cleland1$plot==6,]$block<-3
  cleland1[cleland1$plot==13|cleland1$plot==14|cleland1$plot==26|cleland1$plot==32|cleland1$plot==34,]$block<-4
  cleland1[cleland1$plot==15|cleland1$plot==16|cleland1$plot==17|cleland1$plot==18,]$block<-5
  cleland1[cleland1$plot==19|cleland1$plot==20|cleland1$plot==21|cleland1$plot==31|cleland1$plot==35,]$block<-6
  cleland1[cleland1$plot==27|cleland1$plot==28|cleland1$plot==29|cleland1$plot==30,]$block<-7
  cleland1[cleland1$plot==22|cleland1$plot==23|cleland1$plot==24|cleland1$plot==25|cleland1$plot==36,]$block<-8
  colnames(cleland1)[5]<-"plot2"
  cleland1$plot<-paste(cleland1$plot2,cleland1$quad,sep="-")
  
  cleland<-subset(cleland1, select=c("site","block","plot","event","year","genus","species", "doy"))
  #cleland$variety <- NA
  #cleland$cult <- NA
  cleland<-cleland[!is.na(cleland$doy),]
  
  return(cleland)
}

##Clark et al from Duke ##
## Data type: BBD,LUD, LOD ##
## Notes: Contact: Public data ##
clean.raw$clarkduke <- function(filename, path="./Data/Experiments/clark/") {
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
    clarkduke1$site<-"exp03"
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
    clarkduke4$block<-NA
    clarkduke5<-subset(clarkduke4, select=c("site","block","plot","event","year","genus","species","doy"))
    clarkduke<-rbind(clarkduke,clarkduke5)
  }
  clarkduke<-clarkduke[!is.na(clarkduke$doy),]
  clarkduke<-clarkduke[-which(clarkduke$genus=="Ob"),]#unknown genus at clarkduke
  clarkduke[which(clarkduke$genus=="Carya "),]$genus<-"Carya"
  return(clarkduke)
}

##Clark et al from Harvard ##
## Data type: BBD,LUD,LOD ##
## Notes: Contact: Public data ##
clean.raw$clarkharvard <- function(filename, path="./Data/Experiments/clark") {
  clarkharvardplots<-c("HF_G01_3.csv","HF_G02_A.csv","HF_G03_5.csv","HF_G04_A.csv","HF_G05_5.csv","HF_G06_3.csv","HF_G07_A.csv","HF_G08_3.csv","HF_G09_5.csv","HF_G10_C.csv","HF_G11_C.csv","HF_G12_C.csv","HF_S01_5.csv","HF_S02_A.csv","HF_S03_3.csv","HF_S04_5.csv","HF_S05_A.csv","HF_S06_3.csv","HF_S07_A.csv","HF_S08_3.csv","HF_S09_5.csv","HF_S10_C.csv","HF_S11_C.csv","HF_S12_C.csv")
  clarkharvard <- c()
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
    clarkharvard1$site<-"exp04"      
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
    clarkharvard4$block<-NA
    clarkharvard5<-subset(clarkharvard4, select=c("site","block","plot","event","year","genus","species","doy"))
    clarkharvard<-rbind(clarkharvard,clarkharvard5)
  }
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
  sp<-c("millefolium","psilostachya","gerardii","strigosus","virgatum","scoparium")
  for (i in 1:length(sherryspp)){
    file <- file.path(path, paste(sherryspp[i]))
    sherry1 <- read.csv(file, skip=3, header=TRUE)
    colnames(sherry1)[which(colnames(sherry1)=="Plot")]<-"plot"
    #estimate first date of flowering and fruiting
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
  #add in other file,which has different species that are little different than others in formatting
  file2 <- file.path(path, "SherryPhenology2003_First6spp.csv")
  sherry5 <- read.csv(file2, skip=3, header=TRUE)
  verarv<-sherry5[1:40,]
  viobic<-sherry5[44:82,]
  colnames(viobic)<-c(sherry5[43,])
  cerglo<-sherry5[86:119,]
  colnames(cerglo)<-c(sherry5[85,])
  plavir<-sherry5[123:158,]
  colnames(plavir)<-c(sherry5[122,])
  broarv<-sherry5[162:201,]
  colnames(broarv)<-c(sherry5[161,])
  dicoli<-sherry5[205:235,]
  colnames(dicoli)<-c(sherry5[204,])
  get.ffd <- function(x) names(x)[min(which(x <= 3.5 & x >= 2.5), na.rm=T)]#first flower date
  get.ffrd <- function(x) names(x)[min(which(x <= 5.5 & x >= 3.5), na.rm=T)]#leaves unfolding
  dicoli_ffd_doy<-substr(apply(dicoli[,5:14],1,get.ffd),2,4)
  dicoli_ffrd_doy<-substr(apply(dicoli[,5:14],1,get.ffrd),2,4)
  dicoli2<-cbind(dicoli,dicoli_ffd_doy,dicoli_ffrd_doy)
  dicoli2$genus<-"Dichanthelium" 
  dicoli2$species<-"oligosanthes"   
  colnames(dicoli2)[1]<-"plot"
  colnames(dicoli2)[16]<-"ffd_doy"
  colnames(dicoli2)[17]<-"ffrd_doy"
  dicoli3<-subset(dicoli2, select=c("plot","genus","species", "ffd_doy","ffrd_doy"))
  broarv_ffd_doy<-substr(apply(broarv[,5:14],1,get.ffd),1,4)
  broarv_ffrd_doy<-substr(apply(broarv[,5:14],1,get.ffrd),1,4)
  broarv_ffd_doy[which(substr(broarv_ffd_doy,1,1)=="d")]<-substr(broarv_ffd_doy[which(substr(broarv_ffd_doy,1,1)=="d")],2,4)
  broarv_ffrd_doy[which(substr(broarv_ffrd_doy,1,1)=="d")]<-substr(broarv_ffrd_doy[which(substr(broarv_ffrd_doy,1,1)=="d")],2,4)
  broarv2<-cbind(broarv,broarv_ffd_doy,broarv_ffrd_doy)
  broarv2$genus<-"Bromus" 
  broarv2$species<-"arvensis"   
  colnames(broarv2)[1]<-"plot"
  colnames(broarv2)[16]<-"ffd_doy"
  colnames(broarv2)[17]<-"ffrd_doy"
  broarv3<-subset(broarv2, select=c("plot","genus","species", "ffd_doy","ffrd_doy"))
  cerglo_ffd_doy<-substr(apply(cerglo[,5:10],1,get.ffd),2,7)
  cerglo_ffrd_doy<-substr(apply(cerglo[,5:10],1,get.ffrd),2,7)
  cerglo_ffd_doy[which(substr(cerglo_ffd_doy,1,2)=="ay")]<-substr(cerglo_ffd_doy[which(substr(cerglo_ffd_doy,1,2)=="ay")],3,5)
  cerglo_ffrd_doy[which(substr(cerglo_ffrd_doy,1,2)=="ay")]<-substr(cerglo_ffrd_doy[which(substr(cerglo_ffrd_doy,1,2)=="ay")],3,5)
  cerglo2<-cbind(cerglo,cerglo_ffd_doy,cerglo_ffrd_doy)
  cerglo2$genus<-"Cerastium" 
  cerglo2$species<-"glomeratum"   
  colnames(cerglo2)[1]<-"plot"
  colnames(cerglo2)[16]<-"ffd_doy"
  colnames(cerglo2)[17]<-"ffrd_doy"
  cerglo3<-subset(cerglo2, select=c("plot","genus","species", "ffd_doy","ffrd_doy"))
  plavir_ffd_doy<-substr(apply(plavir[,5:14],1,get.ffd),2,4)
  plavir_ffrd_doy<-substr(apply(plavir[,5:14],1,get.ffrd),2,4)
  plavir2<-cbind(plavir,plavir_ffd_doy,plavir_ffrd_doy)
  plavir2$genus<-"Plantago" 
  plavir2$species<-"virginica"   
  colnames(plavir2)[1]<-"plot"
  colnames(plavir2)[16]<-"ffd_doy"
  colnames(plavir2)[17]<-"ffrd_doy"
  plavir3<-subset(plavir2, select=c("plot","genus","species", "ffd_doy","ffrd_doy"))
  viobic_ffd_doy<-substr(apply(viobic[,5:14],1,get.ffd),2,4)
  viobic_ffrd_doy<-substr(apply(viobic[,5:14],1,get.ffrd),2,4)
  viobic2<-cbind(viobic,viobic_ffd_doy,viobic_ffrd_doy)
  viobic2$genus<-"Viola" 
  viobic2$species<-"bicolor"   
  colnames(viobic2)[1]<-"plot"
  colnames(viobic2)[16]<-"ffd_doy"
  colnames(viobic2)[17]<-"ffrd_doy"
  viobic3<-subset(viobic2, select=c("plot","genus","species", "ffd_doy","ffrd_doy"))
  verarv_ffd_doy<-substr(apply(verarv[,5:14],1,get.ffd),4,6)
  verarv_ffrd_doy<-substr(apply(verarv[,5:14],1,get.ffrd),4,6)
  verarv2<-cbind(verarv,verarv_ffd_doy,verarv_ffrd_doy)
  verarv2$genus<-"Veronica" 
  verarv2$species<-"arvensis"   
  colnames(verarv2)[1]<-"plot"
  colnames(verarv2)[16]<-"ffd_doy"
  colnames(verarv2)[17]<-"ffrd_doy"
  verarv3<-subset(verarv2, select=c("plot","genus","species", "ffd_doy","ffrd_doy"))
  sherry6<-rbind(broarv3,dicoli3,verarv3,viobic3,plavir3,cerglo3)
  sherry7<-reshape(sherry6,varying = list(names(sherry6)[4:5]), direction = "long", v.names = c("doy"), times = c(1:2))
  sherry7$event<-c(rep("ffd", times=dim(sherry6)[1]),rep("ffrd", times=dim(sherry6)[1]))
  
  sherry8<-rbind(sherry4,sherry7)
  sherry8$year<-2003
  sherry8$site<-"exp12"
  sherry8$block<-NA
  sherry8[as.numeric(sherry8$plot)<5,]$block<-1
  sherry8[as.numeric(sherry8$plot)<11 & as.numeric(sherry8$plot)>6,]$block<-2
  sherry8[as.numeric(sherry8$plot)<15 & as.numeric(sherry8$plot)>10,]$block<-3
  sherry8[as.numeric(sherry8$plot)==5|as.numeric(sherry8$plot)==6|as.numeric(sherry8$plot)==15|as.numeric(sherry8$plot)==16,]$block<-4
  sherry8[as.numeric(sherry8$plot)>16,]$block<-5
  sherryok<-subset(sherry8, select=c("site","block","plot","event","year","genus","species", "doy"))
  #sherryok$variety <- NA
  #sherryok$cult <- NA
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
#The number of files should correspond more-or-less with the number of years reported in the 1998 paper, with the addition of a few files from 1990 censuses (which can probably be left out of analyses since we were working the bugs out of our methods in that year and this was not a warming year).
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
    price1<-price1[!(price1$plot==""),]
    price1<-price1[!(price1$plot=="JDATE"),]
    price1<-price1[!(price1$plot=="PLOT"),]
    price1<-price1[!(price1$plot=="jdate"),]
    price1<-price1[!(price1$plot=="1992 WARMED-MEADOW PHENOLOGY"),]
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
  price4$site<-"exp11"
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
  price4$block<-NA
  price5<-subset(price4, select=c("site","block", "plot","event","year","genus","species", "doy"))
  price6<-price5[which(price5$year!="1990"),]#remove 1990, since this is prewarming data
  #price5$variety <- NA
  #price5$cult <- NA
  return(price6)
}

##Data from Chuine
##no plots listed for 2003
clean.raw$chuine <- function(filename, path="./Data/Experiments/chuine") {
  chuinefiles<-c("Chuine_pheno_2002.csv","Chuine_pheno_2003_cleaned.csv","Chuine_pheno_2004.csv")
  years<-c(2002,2003,2004)
  chuine <- NA
  for (i in 1:length(chuinefiles)){
    file <- file.path(path, paste(chuinefiles[i]))
    chuine1 <- read.csv(file, header=TRUE)
    colnames(chuine1)[which(colnames(chuine1)=="Block")]<-"block"
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
    chuine3<-subset(chuine2, select=c("block","plot","sp", "event","year","doy"))
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
  chuine$genus[chuine$sp=="lp3"] <- "Lolium"
  chuine$species[chuine$sp=="lp3"] <- "perenne"
  chuine$site<-"exp02"
  chuine4<-subset(chuine, select=c("site","block","plot","event","year","genus","species", "doy"))
  #chuine4$variety <- NA
  #chuine4$cult <- NA
  chuine4<-chuine4[!is.na(chuine4$doy),]
  return(chuine4)
  ##
}
##Data from FORCE
##Contact: Christy Rollinson
clean.raw$force <- function(filename="FORCE_Inventories_2009_2010_clean.csv", path="./Data/Experiments/force") {
  file <- file.path(path, filename)
  force1 <- read.csv(file, check.names=FALSE, header=TRUE)
  force1$plot<-paste(force1$Block,force1$Treatment,sep="")
  force1$block<-force1$Block
  force2<-aggregate(x=force1$Survey.DOY, by=list(force1$Year,force1$block,force1$plot,force1$Species,force1$Phenology.State), FUN=min, na.rm=F)
  colnames(force2)<-c("year","block","plot","SP","phenstate","doy")
  force2$event<-NA
  force2[force2$phenstate=="1",]$event<-"lod"
  force2[force2$phenstate==2,]$event<-"ffd"
  force2[force2$phenstate==3,]$event<-"ffrd"
  force2[force2$phenstate==4,]$event<-"sd"
  force2[force2$phenstate==5,]$event<-"sen"
  force3<-force2[-which(is.na(force2$event)),]
  force3<-force3[-which(force3$SP=="11-Oct"),]
  force3<-force3[-which(force3$SP=="9-Oct"),]
  force3<-force3[-which(force3$SP=="CEOB"),]
  force3<-force3[-which(force3$SP=="U44"),]
  spfile <- file.path(path, "Species_List.csv")
  specieslist<-read.csv(spfile, header=TRUE)
  force3$genussp<-NA
  species1<-unique(force3$SP)
  species1[which(species1=="U80")]<-"ARMI"
  for (j in 1:length(species1)){
    force3$genussp[force3$SP==species1[j]] <- specieslist[specieslist$Species.CODE==species1[j],]$Species[1]
  }
  force4<-force3 %>% separate(genussp, c("genus", "species"), sep=" ", remove=F)
  force4[which(force4$genus=="Sisynchium"),]$genus<-"Sisyrinchium"
  force4[which(force4$genus=="Dianthus"),]$species<-"armeria"
  force4[which(force4$genus=="Amphicarpa"),]$genus<-"Amphicarpaea"
  force4[which(force4$genus=="Actea"),]$genus<-"Actaea"
  force4[which(force4$species=="soria"),]$species<-"sororia"
  force4[which(force4$species=="lavae"),]$species<-"laeve"
  force4[which(force4$species=="abortivis"),]$species<-"abortivus"
  force4[which(force4$species=="gramanifolia"),]$species<-"graminifolia"
  force4[which(force4$genus=="Rubus"),]$species<-"occidentalis"
  force4[which(force4$species=="anuus"),]$species<-"annuus"
  force4[which(force4$genus=="Ceanothus"),]$species<-"americanus"
  force4[which(force4$species=="hieraciifolia"),]$species<-"hieraciifolius"
  #force4[which(force4$genus=="Oenethera"),]$genus<-"Oenothera"
  force4$site<-"exp09"
  force<-subset(force4, select=c("site","block","plot","event","year","genus","species", "doy"))
  return(force)
  ##
}

##Data from Aaron Ellison's warming/phenology/ant experiment at Harvard Forest
##Spring and Fall phenology
##Contact: Aaron Ellison
clean.raw$ellison <- function(filename="hf113-27-hf-phenology.csv", path="./Data/Experiments/ellison") {
  file <- file.path(path, filename)
  ellison1 <- read.csv(file, check.names=FALSE, header=TRUE)
  colnames(ellison1)[2]<-"plot"
  colnames(ellison1)[4]<-"genussp"
  ellison1$doy<-strftime(strptime(ellison1$date, format = "%m/%d/%y"),format = "%j") 
  ellison1$year<-strftime(strptime(ellison1$date, format = "%m/%d/%y"),format = "%Y")
  ellison2<-aggregate(x=ellison1$doy, by=list(ellison1$year,ellison1$plot,ellison1$genussp,ellison1$plant,ellison1$phenology), FUN=min,na.rm=F)
  ellison3<-ellison2 %>% separate(Group.3, c("genus", "species"), sep="_", remove=F)
  colnames(ellison3)<-c("year","plot","gensp")
  colnames(ellison3)[4:8]<-c("genus","species","plant","phenology","doy")
  ellison3$event<-NA
  ellison3[ellison3$phenology=="F2",]$event<-"sen"
  ellison3[ellison3$phenology=="F3",]$event<-"drop"
  ellison3[ellison3$phenology=="S3",]$event<-"bbd"
  ellison3[ellison3$phenology=="S4",]$event<-"lud"
  ellison3[ellison3$phenology=="S5",]$event<-"lod"
  ellison3[which(ellison3$plot=="13"|ellison3$plot=="14"|ellison3$plot=="15"),]$plot<-"OUT"
  ellison3$site<-"exp07"
  ellison3$block<-NA
  ellison<-subset(ellison3, select=c("site","block","plot","event","year","genus","species", "doy"))
  return(ellison)
}


##Data from Jennifer Dunne's study at RMBL
##Phenological stages:0=not yet flowering, 1=unopened flower buds, 2=open flowers,3 =old flowers,4=initiated fruit, 5=enlarged fruit, and 6= for dehisced fruit. 
#For Festuca we used five phenological stages: 0=plant with flower stalks,1=presence of spikelets, 2=exerted anthers and styles from the spikelet florets, 3=dried and broken-off anthers and styles, indicating a developing seed, and 4=disarticulated seeds.
##Contact: Jennifer Dunne
clean.raw$dunne <- function(path="./Data/Experiments/dunne") {
  dunnefiles<-c("1995DunnePhenologyData_Artemisia.csv","1995DunnePhenologyData_Delphinium.csv","1995DunnePhenologyData_Erigeron.csv","1995DunnePhenologyData_Helianthella.csv","1995DunnePhenologyData_Lathyrus.csv","1995DunnePhenologyData_Mertensiana.csv","1995DunnePhenologyData_Potentilla.csv","1996DunnePhenologyData_Artemisia.csv","1996DunnePhenologyData_Delphinium.csv","1996DunnePhenologyData_Erigeron.csv","1996DunnePhenologyData_Eriogonums.csv","1996DunnePhenologyData_Festuca.csv","1996DunnePhenologyData_Helianthella.csv","1996DunnePhenologyData_Lathyrus.csv","1996DunnePhenologyData_Mertensiana.csv","1996DunnePhenologyData_Potentilla.csv","1997DunnePhenologyData_Achillea.csv","1997DunnePhenologyData_Artemisia.csv","1997DunnePhenologyData_Claytonia.csv","1997DunnePhenologyData_Delphinium.csv","1997DunnePhenologyData_Erigeron.csv","1997DunnePhenologyData_Eriogonumu.csv","1997DunnePhenologyData_Festuca.csv","1997DunnePhenologyData_Helianthella.csv","1997DunnePhenologyData_Lathyrus.csv","1997DunnePhenologyData_Mertensia.csv","1997DunnePhenologyData_Potentilla.csv","1998DunnePhenologyData_Artemisia.csv","1998DunnePhenologyData_Claytonia.csv","1998DunnePhenologyData_Delphinium.csv","1998DunnePhenologyData_Erigeron.csv","1998DunnePhenologyData_Eriogonumu.csv","1998DunnePhenologyData_Eriogonums.csv","1998DunnePhenologyData_Festuca.csv","1998DunnePhenologyData_Helianthella.csv","1998DunnePhenologyData_Lathyrus.csv","1998DunnePhenologyData_Mertensia.csv","1998DunnePhenologyData_Potentilla.csv")
  dunne <- NA
  for (i in 1:length(dunnefiles)){
    file <- file.path(path, paste(dunnefiles[i]))
    dunne1 <- read.csv(file,  header=TRUE)
    dunne_ffd<-aggregate(x=dunne1$date, by=list(dunne1$site,dunne1$plot,dunne1$rep,dunne1$stage2), FUN=min,na.rm=F)#first date of open flowers for each site/plot/rep
    dunne_ffd$event<-"ffd"
    if(is.element("stage4", colnames(dunne1)))(dunne_ffrd<-aggregate(x=dunne1$date, by=list(dunne1$site,dunne1$plot,dunne1$rep,dunne1$stage4), FUN=min,na.rm=F)) #first date of fruit for each site/plot/rep
    else(dunne2<-dunne_ffd)
    if(is.element("stage4", colnames(dunne1)))(dunne_ffrd$event<-"ffrd")
    if(is.element("stage4", colnames(dunne1)))(dunne2<-rbind(dunne_ffd, dunne_ffrd))
    dunne2$plot<-dunne2$Group.2
    stop<-nchar(dunnefiles[i])-4
    dunne2$genussp<-paste(substr(dunnefiles[i],24,stop))
    dunne2$year<-substr(dunnefiles[i],1,4)
    colnames(dunne2)[5]<-c("doy")
    dunne <- rbind(dunne,dunne2)
  }
  dunne$genus<-NA 
  dunne$species<-NA
  dunne$genus[dunne$genussp=="Artemisia"] <- "Artemisia"
  dunne$species[dunne$genussp=="Artemisia"] <- "tridentata"
  dunne$genus[dunne$genussp=="Claytonia"] <- "Claytonia"
  dunne$species[dunne$genussp=="Claytonia"] <- "lanceolata"
  dunne$genus[dunne$genussp=="Delphinium"] <- "Delphinium"
  dunne$species[dunne$genussp=="Delphinium"] <- "nuttallianum"
  dunne$genus[dunne$genussp=="Erigeron"] <- "Erigeron"
  dunne$species[dunne$genussp=="Erigeron"] <- "speciosus"
  dunne$genus[dunne$genussp=="Helianthella"] <- "Helianthella"
  dunne$species[dunne$genussp=="Helianthella"] <- "quinquenervis"
  dunne$genus[dunne$genussp=="Lathyrus"] <- "Lathyrus"
  dunne$species[dunne$genussp=="Lathyrus"] <- "lanszwertii"
  dunne$genus[dunne$genussp=="Potentilla"] <- "Potentilla"
  dunne$species[dunne$genussp=="Potentilla"] <- "hippiana"
  dunne$genus[dunne$genussp=="Mertensiana"] <- "Mertensiana"
  dunne$species[dunne$genussp=="Mertensiana"] <- "fusiformis"
  dunne$genus[dunne$genussp=="Eriogonums"] <- "Eriogonum"
  dunne$species[dunne$genussp=="Eriogonums"] <- "subalpinum"
  dunne$genus[dunne$genussp=="Festuca"] <- "Festuca"
  dunne$species[dunne$genussp=="Festuca"] <- "thurberi"
  dunne$genus[dunne$genussp=="Achillea"] <- "Achillea"
  dunne$species[dunne$genussp=="Achillea"] <- "sp"
  dunne$genus[dunne$genussp=="Eriogonumu"] <- "Eriogonum"
  dunne$species[dunne$genussp=="Eriogonumu"] <- "umbellatum"
  dunne$site<-"exp06"
  #colnames(dunne)[1]<-"block"#this is the "site" column from the dunne files. i think we actually want to select out only plots frmo one site...
  dunne$block<-NA
  dunne<-dunne[-1,]
  dunne<-dunne[dunne$Group.1=="4",]#site 4= the warming meadow, so we only want these data
  dunnermbl<-subset(dunne, select=c("site","block","plot","event","year","genus","species", "doy"))
  dunnermbl<-dunnermbl[!is.na(dunnermbl$genus),]
  return(dunnermbl)
}
##Data from Haibei Alpine Grassland Research Station, China
##Spring phenology
##Contact: sonamkyi@itpcas.ac.cn
clean.raw$haibei <- function(filename="ww_data1.csv", path="./Data/Experiments/haibei") {
  file <- file.path(path, filename)
  haibei1 <- read.csv(file, check.names=FALSE, header=TRUE)
  haibei1<-haibei1[haibei1$Treatment!="WW",]#remove winter warming treatment
  colnames(haibei1)[1]<-"year"
  colnames(haibei1)[4]<-"block"
  colnames(haibei1)[5]<-"plot"
  flow<-subset(haibei1,select=c("block","plot","year","Species","FFD"))
  colnames(flow)[5]<-"doy"
  flow$event<-"ffd"
  leaf<-subset(haibei1,select=c("block","plot","year","Species","LOD"))
  colnames(leaf)[5]<-"doy"
  leaf$event<-"lod"
  haibei<-rbind(flow,leaf)
  haibei$genus[haibei$Species=="En"] <- "Elymus"
  haibei$species[haibei$Species=="En"] <- "nutans"
  haibei$genus[haibei$Species=="Sa"] <- "Stipa"
  haibei$species[haibei$Species=="Sa"] <- "alinea"
  haibei$genus[haibei$Species=="Pp"] <- "Poa"
  haibei$species[haibei$Species=="Pp"] <- "pratensis"
  haibei$genus[haibei$Species=="Kh"] <- "Kobresia"
  haibei$species[haibei$Species=="Kh"] <- "humilis"
  haibei$genus[haibei$Species=="Th"] <- "Tibetia"
  haibei$species[haibei$Species=="Th"] <- "himalaica"
  haibei$genus[haibei$Species=="Ma"] <- "Melilotoides"
  haibei$species[haibei$Species=="Ma"] <- "archiducis-nicolai"
  haibei$genus[haibei$Species=="Pn"] <- "Potentilla"
  haibei$species[haibei$Species=="Pn"] <- "nivea"
  haibei$genus[haibei$Species=="Gs"] <- "Gentiana"
  haibei$species[haibei$Species=="Gs"] <- "straminea"
  haibei$genus[haibei$Species=="Ss"] <- "Saussurea"
  haibei$species[haibei$Species=="Ss"] <- "superba"
  haibei$genus[haibei$Species=="Gl"] <- "Gentiana"
  haibei$species[haibei$Species=="Gl"] <- "lawrencei"
  haibei$genus[haibei$Species=="Ad"] <- "Aster"
  haibei$species[haibei$Species=="Ad"] <- "diplostephioides"
  haibei$site<-"exp13"
  haibei$doy<-round(haibei$doy,digits=0)
  haibei2<-subset(haibei, select=c("site","block","plot","event","year","genus","species", "doy"))
  return(haibei2)
}

##Data from Cedar Creek
##Spring and Fall phenology
##Contact: danbaha@umn.edu
clean.raw$cc <- function(filename="https___pasta.lternet.edu_package_data_eml_knb-lter-cdr_575_8_760e44559a2611967d61bb35f21d9260.csv", path="./Data/Experiments/cedarcreek") {
  file <- file.path(path, filename)
  phen <- read.csv(file, check.names=FALSE, header=TRUE)
  colnames(phen)[2]<-"year"
  colnames(phen)[4]<-"block"
  phen$Heat.treatment<-NA#make the plots to match those in clim data
  phen$Heat.treatment[phen$Treatment=="0-Ambient"]<-"control"
  phen$Heat.treatment[phen$Treatment=="1-Low"]<-"low"
  phen$Heat.treatment[phen$Treatment=="2-High"]<-"high"
  phen$plot<-paste(phen$block,phen$Heat.treatment,sep="-")
  flow<-subset(phen,select=c("block","plot","year","Species","FlwrDay"))
  colnames(flow)[5]<-"doy"
  flow$event<-"ffd"
  fruit<-subset(phen,select=c("block","plot","year","Species","SeedDay"))
  colnames(fruit)[5]<-"doy"
  fruit$event<-"ffrd"
  ccphen<-rbind(flow,fruit)
  ccphen$site<-"exp14"
  genus.species<-strsplit(ccphen$Species," ")
  genus.species<-do.call(rbind, genus.species)
  colnames(genus.species)<-c("genus","species")
  ccphen<-cbind(ccphen,genus.species)
  cedarcreek<-subset(ccphen, select=c("site","block","plot","event","year","genus","species", "doy"))
  return(cedarcreek)
}

# Produce cleaned raw data
raw.data.dir <- "./Data/Experiments"
cleandata.raw <- list()
cleandata.raw$marchin <- clean.raw$marchin(path="./Data/Experiments/marchin")
cleandata.raw$bace <- clean.raw$bace(path="./Data/Experiments/bace")
cleandata.raw$farnsworth <- clean.raw$farnsworth(path="./Data/Experiments/farnsworth")
cleandata.raw$cleland <- clean.raw$cleland(path="./Data/Experiments/cleland")
cleandata.raw$clarkduke <- clean.raw$clarkduke(path="./Data/Experiments/clark")
cleandata.raw$clarkharvard <- clean.raw$clarkharvard(path="./Data/Experiments/clark")
cleandata.raw$sherry <- clean.raw$sherry(path="./Data/Experiments/sherry")
cleandata.raw$price <- clean.raw$price(path="./Data/Experiments/price")
cleandata.raw$chuine<- clean.raw$chuine(path="./Data/Experiments/chuine")
cleandata.raw$force<- clean.raw$force(path="./Data/Experiments/force")
cleandata.raw$ellison<- clean.raw$ellison(path="./Data/Experiments/ellison")
cleandata.raw$dunne<- clean.raw$dunne(path="./Data/Experiments/dunne")
cleandata.raw$haibei<- clean.raw$haibei(path="./Data/Experiments/haibei")
cleandata.raw$cc<- clean.raw$cc(path="./Data/Experiments/cedarcreek")

expphendb <- do.call("rbind", cleandata.raw)
row.names(expphendb) <- NULL
#Do some additional cleaning and checking:
dim(expphendb)
#76966     8
expphendb<-expphendb[!is.na(expphendb$event),]
expphendb<-expphendb[!is.na(expphendb$doy),]
expphendb$doy<-as.numeric(expphendb$doy)
dim(expphendb)#75314  rows,8 columns
expphendb<-expphendb[!is.na(expphendb$genus),]
expphendb<-expphendb[!expphendb$genus=="",]
expphendb<-expphendb[!expphendb$genus=="spp.",]#should look at these
expphendb<-expphendb[-which(expphendb$genus=="Le"),]#should look at these
expphendb<-expphendb[-which(expphendb$genus=="Unknown"),]#should look at these
expphendb[which(expphendb$genus=="Artemesia"),]$genus<-"Artemisia"#chuine
expphendb[which(expphendb$species=="spp"),]$species<-"sp"#force
expphendb[which(expphendb$species=="spp."),]$species<-"sp"#force
expphendb[which(expphendb$species=="sp."),]$species<-"sp"#force
expphendb[which(expphendb$doy=="144153"),]$doy<-"153"#bace
expphendb[which(expphendb$species=="officionale"),]$species<-"officinale"#force
expphendb[which(expphendb$species=="(incanum?)"),]$species<-"incanum"#force
expphendb[which(expphendb$species=="quiquefolia"),]$species<-"quinquefolia"#force
expphendb[which(expphendb$species=="fusiformes"),]$species<-"fusiformis"#price
expphendb[which(expphendb$genus=="Mertensiana"),]$genus<-"Mertensia"#price
expphendb[which(expphendb$species=="caepitosum"),]$species<-"caespitosum"#force
expphendb[which(expphendb$genus=="Avena"),]$species<-"sp"#JAsper ridge- could be multiple spp
expphendb[which(expphendb$species==""),]$species<-"sp"#all galiums at force

dim(expphendb)#72468 rows,8 columns
head(expphendb)
expphendb <- expphendb[order(expphendb$site,expphendb$block,expphendb$plot,expphendb$year,expphendb$doy,expphendb$genus),] 
write.csv(expphendb,"analyses/exppheno.csv",row.names=F, eol="\r\n")

sort(unique(expphendb$site))#14 experiments across 9 sites
sort(unique(expphendb$genus))#161 genera
expphendb$genus.species<-paste(expphendb$genus,expphendb$species,sep=".")
sort(unique(expphendb$genus.species))#268 species

unique(expphendb$event)#13 phenological events
#Do species cleaning with Miriam's new file
#sites<-unique(expphendb$site)
#for(i in 1:length(sites)){
#for (j in 1:length(species1)){
# clarkduke1$genus[clarkduke1$Species==species1[j]] <- specieslist[specieslist$shortCode==species1[j],]$genus
#clarkduke1$species[clarkduke1$Species==species1[j]] <- specieslist[specieslist$shortCode==species1[j],]$species
#}
#}

specieslist<-sort(unique(paste(expphendb$genus,expphendb$species, sep=".")))
write.csv(specieslist,"exp_splist.csv")

