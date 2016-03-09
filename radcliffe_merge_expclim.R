### Started 8 March 2016 ##
### By Ailene Ettinger ###
setwd("~/GitHub/radcliffe")
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(reshape)
library(zoo)
##Daily temp  data and whatever soil moisture data are available
clean.clim <- list()

clean.clim$marchin <- function(filename="hf113-10-df-chamber.csv",path="./Experiments/marchin") {
  
  ## Marchin ##
  ## Data type: temp (air and soil), soil moisture, in chambers (incuding 3 unheated control chambers, 9 heated chambers, and 3 outside controls that lack chambers); regression heating design
  ## Notes: Contact: Renee Marchin, renee.marchin@sydney.edu.au##
  file <- file.path(path, filename)
  marchin1 <- read.csv(file, check.names=FALSE, header=TRUE)
  names(marchin1)[8]<-"plot"
  marchin1$year_doy<-paste(marchin1$year,marchin1$doy, sep="-")
  airtemp_mn1<-tapply(marchin1$CAT1_Avg,list(marchin1$year_doy,marchin1$plot), mean, na.rm=T)#mean daily air temperature, sensor 1
  airtemp_mn2<-tapply(marchin1$CAT2_Avg,list(marchin1$year_doy,marchin1$plot), mean, na.rm=T)#mean daily air temperature, sensor 2
  airtemp_mn3<-tapply(marchin1$CAT3_Avg,list(marchin1$year_doy,marchin1$plot), mean, na.rm=T)#mean daily air temperature, sensor 3
  sotemp_mn1<-tapply(marchin1$CSTo1_Avg,list(marchin1$year_doy,marchin1$plot), mean, na.rm=T)#mean daily soil (organic) temperature, sensor1, buried 2 cm below the surface
  sotemp_mn2<-tapply(marchin1$CSTo2_Avg,list(marchin1$year_doy,marchin1$plot), mean, na.rm=T)#mean daily soil (organic) temperature, sensor 2,buried 2 cm below the surface
  sitemp_mn1<-tapply(marchin1$CSTI1_Avg,list(marchin1$year_doy,marchin1$plot), mean, na.rm=T)#mean daily soil (inorganic) temperature, sensor 1,buried 6 cm below the surface
  sitemp_mn2<-tapply(marchin1$CSTI2_Avg,list(marchin1$year_doy,marchin1$plot), mean, na.rm=T)#mean daily soil (inorganic) temperature, sensor 2,buried 6 cm below the surface
  soilmois<-tapply(marchin1$CSM_Avg,list(marchin1$year_doy,marchin1$plot), mean, na.rm=T)#mean daily soil moisture
  year_doy <- strsplit(rownames(airtemp_mn1),'-') 
  year_doy<-do.call(rbind, year_doy)
  allclim<-as.data.frame(cbind(year_doy,airtemp_mn1,airtemp_mn2,airtemp_mn3,sotemp_mn1,sotemp_mn2,sitemp_mn1,sitemp_mn2,soilmois))
  names<-c(rep("airtemp_mn1", times=12),rep("airtemp_mn2", times=12),rep("airtemp_mn3", times=12),rep("sotemp_mn1",times=12),rep("sotemp_mn2",times=12),rep("sitemp_mn1",times=12),rep("sitemp_mn2",times=12),rep("soilmois",times=12))
  newcolnames<-NA
  for (i in 1:length(names)){
   newcolnames[i]<-paste(names[i],"_plot",colnames(allclim)[2+i],sep="")
  }
  colnames(allclim)<-c("year","doy",newcolnames)
  allclim1<-reshape(allclim,varying = list(colnames(allclim)[3:14], colnames(allclim)[15:26],colnames(allclim)[27:38],colnames(allclim)[39:50],colnames(allclim)[51:62],colnames(allclim)[63:74],colnames(allclim)[75:86],colnames(allclim)[87:98]), direction = "long", v.names = c("airtemp_mn1","airtemp_mn2", "airtemp_mn3","sotemp_mn1","sotemp_mn2","sitemp_mn1","sitemp_mn2","soilmois"), times = c(1:12))
  colnames(allclim1)[3]<-"plot"
  allclim1$airtemp_mn1<-as.numeric(allclim1$airtemp_mn1)
  allclim1$airtemp_mn2<-as.numeric(allclim1$airtemp_mn2)
  allclim1$airtemp_mn3<-as.numeric(allclim1$airtemp_mn3)
  allclim1$sotemp_mn1<-as.numeric(allclim1$sotemp_mn1)
  allclim1$sotemp_mn2<-as.numeric(allclim1$sotemp_mn2)
  allclim1$sitemp_mn1<-as.numeric(allclim1$sitemp_mn1)
  allclim1$sitemp_mn2<-as.numeric(allclim1$sitemp_mn2)
  temp<-subset(allclim1, select=c("airtemp_mn1","airtemp_mn2","airtemp_mn3"))
  orgsoiltemp<-subset(allclim1, select=c("sotemp_mn1","sotemp_mn2"))
  inorgsoiltemp<-subset(allclim1, select=c("sitemp_mn1","sitemp_mn2"))
  soiltemp<-subset(allclim1, select=c("sotemp_mn1","sotemp_mn2","sitemp_mn1","sitemp_mn2"))
  airtemp<-rowMeans(temp)#not sure if we want to do this? average across all airtemps
  soiltemp<-rowMeans(soiltemp)#not sure if we want to do this? average across all soiltemps
  allclim1<-cbind(allclim1,airtemp,soiltemp)
  allclim1$site<-"marchin"
  allclim1$treatment<-NA
  marchinclim<-subset(allclim1, select=c("site","treatment","plot","year","doy","airtemp","soiltemp","soilmois"))
  row.names(marchinclim) <- NULL
  return(marchinclim)
}
##Farnsworth from Harvard ##
## Data type: soil temp (celsius, at 5cm depth) and soil moisture (% volumetric moisture content) in heated plots (=h=1,6,8,12,15,16), disturbance control plots (=d=3,5,9,10,13,17), and control plots (=c=2,4,7,11,14,18)
## Notes: Contact: Public data,http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf005
clean.clim$farnsworth <- function(filename="hf005-04-soil-temp.csv", path="./Experiments/farnsworth/") {
  file <- file.path(path, filename)
  temp<- read.csv(file, check.names=FALSE, header=TRUE)
  temp.long<-reshape(temp,varying = list(colnames(temp)[6:23]), direction = "long", v.names = c("soiltemp"), times = c(colnames(temp)[6:23]))
  colnames(temp.long)[10]<-"plotfull"
  temp.long$plot<-substr(temp.long$plotfull,5,6)
  temp.long[temp.long$plotfull=="plot1h",]$plot=1
  temp.long[temp.long$plotfull=="plot6h",]$plot=6
  temp.long[temp.long$plotfull=="plot8h",]$plot=8
  temp.long[temp.long$plotfull=="plot3d",]$plot=3
  temp.long[temp.long$plotfull=="plot5d",]$plot=5
  temp.long[temp.long$plotfull=="plot9d",]$plot=9
  temp.long[temp.long$plotfull=="plot2c",]$plot=2
  temp.long[temp.long$plotfull=="plot4c",]$plot=4
  temp.long[temp.long$plotfull=="plot7c",]$plot=7
  temp.long2<-subset(temp.long,selec=c("year","doy","plot","soiltemp"))
  file2<-file.path(path, "hf005-05-soil-respiration.csv")
  mois<-read.csv(file2,  header=TRUE)
  mois$doy<-strftime(strptime(paste(mois$year,mois$month,mois$day,sep="-"), format = "%Y-%m-%d"),format = "%j") 
  mois2<-subset(mois,selec=c("year","doy","plot","treatment","moisture","temp.4cm"))
  allclim<-merge(temp.long2,mois2,by.x=c("year","doy","plot"),by.y=c("year","doy","plot"),all=TRUE)
  allclim$airtemp<-NA 
  colnames(allclim)[6]<-"soilmois"
  allclim$site<-"farnsworth"
  farnsworthclim<-subset(allclim, select=c("site","treatment","plot","year","doy","airtemp","soiltemp","soilmois"))
  row.names(farnsworthclim) <- NULL
  return(farnsworthclim) 
}

##Produce cleaned, raw climate data
raw.data.dir <- "./Experiments/"
cleanclimdata.raw <- list()
cleanclimdata.raw$marchin <- clean.clim$marchin(path="./Experiments/marchin")
cleanclimdata.raw$farnsworth <- clean.clim$farnsworth(path="./Experiments/farnsworth/")

head(cleanclimdata.raw$farnsworth)
expphenclim <- do.call("rbind", cleanclimdata.raw)
row.names(expphenclim) <- NULL
write.csv(expphenclim, "radmeeting/expclim.csv", row.names=FALSE)
