### Started 8 December 2015 ##
### By Ailene Ettinger ###
setwd("~/Dropbox/Documents/Work/Wolkovich/Radcliffe/Analyses")

options(stringsAsFactors=FALSE)

library(reshape)
library(zoo)
##Want to get several files:
#1. Phenology data: site, plot, event (phen event),year, doy,genus,species,Temp_treat	Precip_treat		onetempchange	precipchange	precipchangetyp)
#site plot event year doy       date            genus            species scrub varetc cult
#2. Species names: site,genus,species,scrub,genus.prescrub,species.prescrub,ipni
#3. Study info: DatasetID  Year	medium	Temp_treat	onetempchange	Precip_treat	precipchange	precipchangetyp
#4. Temperature data
#5. Precipitation/moisture data

# make list to store all the derived dataset cleaning functions
clean.raw <- list()

clean.raw$marchin <- function(filename="Budburst_Marchin.csv", path="./Experiments/",names.only=FALSE) {
  
  ## Marchin ##
  ## Data type: BBD,FFD ##
  ## Notes: Contact: Renee Marchin, renee.marchin@sydney.edu.au##
  file <- file.path(path, filename)
  marchin1 <- read.csv(file, check.names=FALSE, header=TRUE)
  #marchin1<-read.csv("Experiments/Budburst_Marchin.csv", header=T)
  names(marchin1)[2] <- "genusspecies"
  names(marchin1)[1] <- "year"
  names(marchin1)[3] <- "plot"
  names(marchin1)[8] <- "doy"
  marchin1a<- subset(marchin1, select=c("year","genusspecies","plot", "doy"))
  marchin1a$site <- "marchin"
  marchin1a$varetc <- NA
  marchin1a$cult <- NA
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
  marchin$varetc <- NA
  marchin$cult <- NA
  
  return(marchin)
}


##Clark et al from Duke ##
## Data type: FFD ##
## Notes: Contact: Public data ##
##have not done this one yet, as it is in a different form than others (raw observation dates with what observed each date)
clean.raw$clarkduke <- function(filename="data-Duke-Q.csv", path="./Experiments/",names.only=FALSE) {
  
  file <- file.path(path, filename)
  clarkduke1 <- read.csv(file, check.names=FALSE, header=TRUE)
  #clarkduke1<-read.csv("Experiments/data-Duke-Q.csv", header=T)
 
  return(clarkduke)
}

##Clark et al from Harvard ##
## Data type: FFD ##
## Notes: Contact: Public data ##
##have not done this one yet, as it is in a different form than others (raw observation dates with what observed each date)
clean.raw$clarkharv <- function(filename="data-Duke-Q.csv", path="./Experiments/",names.only=FALSE) {
  
  file <- file.path(path, filename)
  clarkharv1 <- read.csv(file, check.names=FALSE, header=TRUE)  
  return(clark)
}
##Farnsworth from Harvard ##
## Data type: FFD ##
## Notes: Contact: Public data ##
##have not done this one yet, as it is in a different form than others (raw observation dates with what observed each date)
clean.raw$farnsworth <- function(filename="hf033-01", path="./Experiments/",names.only=FALSE) {
  
  file <- file.path(path, filename)
  farnsworth1 <- read.csv(file, check.names=FALSE, header=TRUE)  
  return(farnsworth)
}

clean.raw$bace <- function(filename="BACE_deciduous2010_originaltrees.csv", path="./Experiments",names.only=FALSE) {
  
  ## Marchin ##
  ## Data type: BBD,LOD ##
  ## Notes: Contact: Renee Marchin, renee.marchin@sydney.edu.au##
  file <- file.path(path, filename)
  bace1 <- read.csv(file, check.names=FALSE, header=TRUE)
  bace1<-bace1[-1,]
  names(bace1)[5] <- "genusspecies"
  names(bace1)[1] <- "plot"
  names(bace1)[6] <- "doy_bb"
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
  bace3<-rbind(bace1a, bace2a)
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
  bace$varetc <- NA
  bace$cult <- NA
  return(bace)
}
