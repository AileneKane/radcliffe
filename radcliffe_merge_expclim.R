### Started 8 March 2016 ##
### By Ailene Ettinger ###
setwd("~/GitHub/radcliffe")
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(reshape)
library(zoo)
##Daily temp  data and whatever soil moisture data are available

clean.clim$marchin <- function(filename="hf113-10-df-chamber.csv", path="./Experiments/marchin/") {
  
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
  sm_mn<-tapply(marchin1$CSM_Avg,list(marchin1$year_doy,marchin1$plot), mean, na.rm=T)#mean daily soil moisture
  year_doy <- strsplit(rownames(airtemp_mn1),'-') 
  year_doy<-do.call(rbind, year_doy)
  allclim<-as.data.frame(cbind(year_doy,airtemp_mn1,airtemp_mn2,airtemp_mn3,sotemp_mn1,sotemp_mn2,sitemp_mn1,sitemp_mn2,sm_mn))
  names<-c(rep("airtemp_mn1", times=12),rep("airtemp_mn2", times=12),rep("airtemp_mn3", times=12),rep("sotemp_mn1",times=12),rep("sotemp_mn2",times=12),rep("sitemp_mn1",times=12),rep("sitemp_mn2",times=12),rep("sm_mn",times=12))
  newcolnames<-NA
  for (i in 1:length(names)){
   newcolnames[i]<-paste(names[i],"_plot",colnames(allclim)[2+i],sep="")
  }
  colnames(allclim)<-c("year","doy",newcolnames)
  allclim1<-reshape(allclim,varying = list(colnames(allclim)[3:14], colnames(allclim)[15:26],colnames(allclim)[27:38],colnames(allclim)[39:50],colnames(allclim)[51:62],colnames(allclim)[63:74],colnames(allclim)[75:86],colnames(allclim)[87:98]), direction = "long", v.names = c("airtemp_mn1","airtemp_mn2", "airtemp_mn3","sotemp_mn1","sotemp_mn2","sitemp_mn1","sitemp_mn2","sm_mn"), times = c(1:12))
  colnames(allclim1)[3]<-"plot"
  allclim1$airtemp_mn1<-as.numeric(allclim1$airtemp_mn1)
  allclim1$airtemp_mn2<-as.numeric(allclim1$airtemp_mn2)
  allclim1$airtemp_mn3<-as.numeric(allclim1$airtemp_mn3)
  allclim1$sotemp_mn1<-as.numeric(allclim1$sotemp_mn1)
  allclim1$sotemp_mn2<-as.numeric(allclim1$sotemp_mn2)
  temp<-subset(allclim1, select=c("airtemp_mn1","airtemp_mn2","airtemp_mn3"))
  soiltemp<-subset(allclim1, select=c("sotemp_mn1","sotemp_mn2"))
  mnairtemp<-rowMeans(temp)
  mnsoiltemp<-rowMeans(soiltemp)
  
  marchinclim<-subset(allclim1, select=c("site","plot","airtemp","soiltemp","soilmois_org","soilmois_inorg"))
 rowMeans(allclim1[,1:3], na.rm = TRUE, dims = 1)
  return(marchinclim)
}