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
  airtemp<-rowMeans(temp,na.rm=T)#not sure if we want to do this? average across all airtemps
  soiltemp<-rowMeans(soiltemp,na.rm=T)#not sure if we want to do this? average across all soiltemps
  allclim1<-cbind(allclim1,airtemp,soiltemp)
  allclim1$treatment<-"warm"
  allclim1[allclim1$plot==2,]$treatment<-"control"
  allclim1[allclim1$plot==5,]$treatment<-"control"
  allclim1[allclim1$plot==11,]$treatment<-"control"
  allclim2<-subset(allclim1, select=c("treatment","plot","year","doy","airtemp","soiltemp","soilmois"))
  
  file2<-file.path(path, "hf113-11-df-outside.csv")
  marchin2<-read.csv(file2, header=TRUE)
  marchin2$year_doy<-paste(marchin2$year,marchin2$doy, sep="-")
  airtemp_mn1<-tapply(marchin2$oAT1_Avg,marchin2$year_doy, mean, na.rm=T)#mean daily air temperature, outside 1
  airtemp_mn2<-tapply(marchin2$oAT2_Avg,marchin2$year_doy, mean, na.rm=T)#mean daily air temperature, outside 2
  airtemp_mn3<-tapply(marchin2$oAT3_Avg,marchin2$year_doy, mean, na.rm=T)#mean daily air temperature, outside 3
  sotemp_mn1<-tapply(marchin2$oSTo1_Avg,marchin2$year_doy, mean, na.rm=T)#mean daily soil (organic) temperature, sensor1
  sotemp_mn2<-tapply(marchin2$oSTo2_Avg,marchin2$year_doy, mean, na.rm=T)#mean daily soil (organic) temperature, sensor 2
  sotemp_mn3<-tapply(marchin2$oSTo3_Avg,marchin2$year_doy, mean, na.rm=T)#mean daily soil (organic) temperature, sensor 3
  sitemp_mn1<-tapply(marchin2$oSTI1_Avg,marchin2$year_doy, mean, na.rm=T)#mean daily soil (inorganic) temperature, sensor 1,buried 6 cm below the surface
  sitemp_mn2<-tapply(marchin2$oSTI2_Avg,marchin2$year_doy, mean, na.rm=T)#mean daily soil (inorganic) temperature, sensor 2,buried 6 cm below the surface
  sitemp_mn3<-tapply(marchin2$oSTI3_Avg,marchin2$year_doy, mean, na.rm=T)#mean daily soil (inorganic) temperature, sensor 3,buried 6 cm below the surface
#looks like no soil moisture for outside chambers?
  year_doy <- strsplit(names(airtemp_mn1),'-') 
  year_doy<-do.call(rbind, year_doy)
plot<-rep("outside", times=1461)
oallclim<-as.data.frame(cbind(year_doy,plot,airtemp_mn1,airtemp_mn2,airtemp_mn3,sotemp_mn1,sotemp_mn2,sotemp_mn3,sitemp_mn1,sitemp_mn2,sitemp_mn3))
oallclim$soilmois<-NA
colnames(oallclim)[1:2]<-c("year","doy")
oallclim$airtemp_mn1<-as.numeric(oallclim$airtemp_mn1)
oallclim$airtemp_mn2<-as.numeric(oallclim$airtemp_mn2)
oallclim$airtemp_mn3<-as.numeric(oallclim$airtemp_mn3)
oallclim$sotemp_mn1<-as.numeric(oallclim$sotemp_mn1)
oallclim$sotemp_mn2<-as.numeric(oallclim$sotemp_mn2)
oallclim$sotemp_mn3<-as.numeric(oallclim$sotemp_mn3)
oallclim$sitemp_mn1<-as.numeric(oallclim$sitemp_mn1)
oallclim$sitemp_mn2<-as.numeric(oallclim$sitemp_mn2)
oallclim$sitemp_mn3<-as.numeric(oallclim$sitemp_mn3)
temp<-subset(oallclim, select=c("airtemp_mn1","airtemp_mn2","airtemp_mn3"))
orgsoiltemp<-subset(oallclim, select=c("sotemp_mn1","sotemp_mn2","sotemp_mn3"))
inorgsoiltemp<-subset(oallclim, select=c("sitemp_mn1","sitemp_mn2","sitemp_mn3"))
soiltemp<-subset(oallclim, select=c("sotemp_mn1","sotemp_mn2","sotemp_mn3","sitemp_mn1","sitemp_mn2","sitemp_mn3"))
airtemp<-rowMeans(temp, na.rm=T)#not sure if we want to do this? average across all airtemps
soiltemp<-rowMeans(soiltemp,na.rm=T)#not sure if we want to do this? average across all soiltemps
oallclim1<-cbind(oallclim,airtemp,soiltemp)
oallclim1$treatment<-"outside control"
oallclim2<-subset(oallclim1, select=c("treatment","plot","year","doy","airtemp","soiltemp","soilmois"))

allclim3<-rbind(allclim2,oallclim2)
allclim3$site<-"marchin"
marchinclim<-subset(allclim3, select=c("site","treatment","plot","year","doy","airtemp","soiltemp","soilmois"))
  row.names(marchinclim) <- NULL
  return(marchinclim)
}
##Farnsworth from Harvard ##
## Data type: soil temp (celsius, at 5cm depth) and soil moisture (% volumetric moisture content) in heated plots (=h=1,6,8,12,15,16), disturbance control plots (=d=3,5,9,10,13,17), and control plots (=c=2,4,7,11,14,18)
##(no air temp)
## Notes: Contact: Public data,http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf005
clean.clim$farnsworth <- function(filename="hf005-04-soil-temp.csv", path="./Experiments/farnsworth/") {
  file <- file.path(path, filename)
  temp<- read.csv(file, header=TRUE)
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
  colnames(allclim)[5]<-"treatment2"
  allclim$treatment<-NA
  allclim[which(allclim$treatment2=="H"),]$treatment<-"warm"
  allclim[which(allclim$treatment2=="C"),]$treatment<-"control"
  allclim[which(allclim$treatment2=="DC"),]$treatment<-"ambient"
  allclim$site<-"farnsworth"
  farnsworthclim<-subset(allclim, select=c("site","treatment","plot","year","doy","airtemp","soiltemp","soilmois"))
  row.names(farnsworthclim) <- NULL
  return(farnsworthclim) 
}

##Clark et al from Harvard ##
## Data type: hourly air temp, soil temp (celsius, at 5cm depth) and soil moisture (Estimated ratio of volumetric water content, scaled from 0 to 1) in heated plots, "ambient" plots = like disturbance control plots, and control plots with mesh walls, no heating infrastructure
## Notes: Public data, 
clean.clim$clarkharvard <- function(filename="data-Harvard-ST1.csv", path="./Experiments/clark/") {
  file <- file.path(path, filename)
  soiltemp_hr<- read.csv(file, header=TRUE)
  soiltemp.long<-reshape(soiltemp_hr,varying = list(colnames(soiltemp_hr)[8:31]), direction = "long", v.names = c("soiltemp"), times = c(colnames(soiltemp_hr)[8:31]))
  soiltemp.long$plot<-substr(soiltemp.long$time,1,3)
  soiltemp.long$doy<-strftime(strptime(paste(soiltemp.long$year,soiltemp.long$month,soiltemp.long$day,sep="-"), format = "%Y-%m-%d"),format = "%j")
  soiltemp.long$year_doy <- paste(soiltemp.long$year,soiltemp.long$doy, sep="") 
  soiltemp<-tapply(soiltemp.long$soiltemp,list(soiltemp.long$year_doy,soiltemp.long$plot), mean, na.rm=T)#mean daily soil (inorganic) temperature, sensor 3,buried 6 cm below the surface
  soiltemp<-as.data.frame(cbind(substr(rownames(soiltemp),1,4),substr(rownames(soiltemp),5,7),soiltemp))
  colnames(soiltemp)[1:2]<-c("year","doy")
  soiltemp2<-reshape(soiltemp,varying = list(colnames(soiltemp)[3:26]), direction = "long", v.names = c("soiltemp"), times = c(colnames(soiltemp)[3:26]))
  colnames(soiltemp2)[3]<-"plot"
  
  file2<-file.path(path, "data-Harvard-SM.csv")
  mois_hr<-read.csv(file2,  header=TRUE)
  mois.long<-reshape(mois_hr,varying = list(colnames(mois_hr)[8:31]), direction = "long", v.names = c("soilmois"), times = c(colnames(mois_hr)[8:31]))
  mois.long$plot<-substr(mois.long$time,1,3)
  mois.long$doy<-strftime(strptime(paste(mois.long$year,mois.long$month,mois.long$day,sep="-"), format = "%Y-%m-%d"),format = "%j")   
  mois.long$year_doy <- paste(mois.long$year,mois.long$doy, sep="") 
  mois<-tapply(mois.long$soilmois,list(mois.long$year_doy,mois.long$plot), mean, na.rm=T)#mean daily soil (inorganic) temperature, sensor 3,buried 6 cm below the surface
  mois<-as.data.frame(cbind(substr(rownames(mois),1,4),substr(rownames(mois),5,7),mois))
  colnames(mois)[1:2]<-c("year","doy")
  mois2<-reshape(mois,varying = list(colnames(mois)[3:26]), direction = "long", v.names = c("soilmois"), times = c(colnames(mois)[3:26]))
  colnames(mois2)[3]<-"plot"
  
  file3<-file.path(path, "data-Harvard-AT.csv")
  airtemp_hr<-read.csv(file3,  header=TRUE)
  airtemp.long<-reshape(airtemp_hr,varying = list(colnames(airtemp_hr)[8:31]), direction = "long", v.names = c("airtemp"), times = c(colnames(airtemp_hr)[8:31]))
  airtemp.long$plot<-substr(airtemp.long$time,1,3)
  airtemp.long$doy<-strftime(strptime(paste(airtemp.long$year,airtemp.long$month,airtemp.long$day,sep="-"), format = "%Y-%m-%d"),format = "%j")   
  airtemp.long$year_doy <- paste(airtemp.long$year,airtemp.long$doy, sep="") 
  airtemp<-tapply(airtemp.long$airtemp,list(airtemp.long$year_doy,airtemp.long$plot), mean, na.rm=T)#mean daily air
  airtemp<-as.data.frame(cbind(substr(rownames(airtemp),1,4),substr(rownames(airtemp),5,7),airtemp))
  colnames(airtemp)[1:2]<-c("year","doy")
  airtemp2<-reshape(airtemp,varying = list(colnames(airtemp)[3:26]), direction = "long", v.names = c("airtemp"), times = c(colnames(airtemp)[3:26]))
  colnames(airtemp2)[3]<-"plot"
  allclim<-merge(airtemp2,soiltemp2,by=c("year","doy","plot","id"), all.y=TRUE)  
  allclim2<-merge(allclim,mois2,by=c("year","doy","plot","id"), all=TRUE) 
  allclim2$site<-"clarkharvard"
  allclim2$treatment<-NA
  allclim2[allclim2$plot=="G02"|allclim2$plot=="G04"|allclim2$plot=="G07"|allclim2$plot=="S02"|allclim2$plot=="S05"|allclim2$plot=="S07",]$treatment<-"ambient"
  allclim2[allclim2$plot=="G03"|allclim2$plot=="G05"|allclim2$plot=="G09"|allclim2$plot=="S01"|allclim2$plot=="S04"|allclim2$plot=="S09",]$treatment<-"warm5C"
  allclim2[allclim2$plot=="G01"|allclim2$plot=="G06"|allclim2$plot=="G08"|allclim2$plot=="S03"|allclim2$plot=="S06"|allclim2$plot=="S08",]$treatment<-"warm3C"
  allclim2[allclim2$plot=="G10"|allclim2$plot=="G11"|allclim2$plot=="G12"|allclim2$plot=="S10"|allclim2$plot=="S11"|allclim2$plot=="S12",]$treatment<-"warm3C"
  clarkharvardclim<-subset(allclim2, select=c("site","treatment","plot","year","doy","airtemp","soiltemp","soilmois"))
  row.names(clarkharvardclim) <- NULL
  return(clarkharvardclim) 
}
##Clark et al from Duke ##
## Data type: hourly air temp, soil temp (celsius, at 5cm depth) and soil moisture (Estimated ratio of volumetric water content, scaled from 0 to 1) in heated plots, "ambient" plots = like disturbance control plots, and control plots with mesh walls, no heating infrastructure
## Notes: Public data, 
clean.clim$clarkduke <- function(filename="data-Duke-ST.csv", path="./Experiments/clark/") {
  file <- file.path(path, filename)
  soiltemp_hr<- read.csv(file, header=TRUE)
  soiltemp.long<-reshape(soiltemp_hr,varying = list(colnames(soiltemp_hr)[8:31]), direction = "long", v.names = c("soiltemp"), times = c(colnames(soiltemp_hr)[8:31]))
  soiltemp.long$plot<-substr(soiltemp.long$time,1,3)
  soiltemp.long$doy<-strftime(strptime(paste(soiltemp.long$year,soiltemp.long$month,soiltemp.long$day,sep="-"), format = "%Y-%m-%d"),format = "%j")
  soiltemp.long$year_doy <- paste(soiltemp.long$year,soiltemp.long$doy, sep="") 
  soiltemp<-tapply(soiltemp.long$soiltemp,list(soiltemp.long$year_doy,soiltemp.long$plot), mean, na.rm=T)#mean daily soil (inorganic) temperature, sensor 3,buried 6 cm below the surface
  soiltemp<-as.data.frame(cbind(substr(rownames(soiltemp),1,4),substr(rownames(soiltemp),5,7),soiltemp))
  colnames(soiltemp)[1:2]<-c("year","doy")
  soiltemp2<-reshape(soiltemp,varying = list(colnames(soiltemp)[3:26]), direction = "long", v.names = c("soiltemp"), times = c(colnames(soiltemp)[3:26]))
  colnames(soiltemp2)[3]<-"plot"
  
  file2<-file.path(path, "data-Duke-SM.csv")
  mois_hr<-read.csv(file2,  header=TRUE)
  mois.long<-reshape(mois_hr,varying = list(colnames(mois_hr)[8:31]), direction = "long", v.names = c("soilmois"), times = c(colnames(mois_hr)[8:31]))
  mois.long$plot<-substr(mois.long$time,1,3)
  mois.long$doy<-strftime(strptime(paste(mois.long$year,mois.long$month,mois.long$day,sep="-"), format = "%Y-%m-%d"),format = "%j")   
  mois.long$year_doy <- paste(mois.long$year,mois.long$doy, sep="") 
  mois<-tapply(mois.long$soilmois,list(mois.long$year_doy,mois.long$plot), mean, na.rm=T)#mean daily soil (inorganic) temperature, sensor 3,buried 6 cm below the surface
  mois<-as.data.frame(cbind(substr(rownames(mois),1,4),substr(rownames(mois),5,7),mois))
  colnames(mois)[1:2]<-c("year","doy")
  mois2<-reshape(mois,varying = list(colnames(mois)[3:26]), direction = "long", v.names = c("soilmois"), times = c(colnames(mois)[3:26]))
  colnames(mois2)[3]<-"plot"
  
  file3<-file.path(path, "data-Duke-AT.csv")
  airtemp_hr<-read.csv(file3,  header=TRUE)
  airtemp.long<-reshape(airtemp_hr,varying = list(colnames(airtemp_hr)[8:31]), direction = "long", v.names = c("airtemp"), times = c(colnames(airtemp_hr)[8:31]))
  airtemp.long$plot<-substr(airtemp.long$time,1,3)
  airtemp.long$doy<-strftime(strptime(paste(airtemp.long$year,airtemp.long$month,airtemp.long$day,sep="-"), format = "%Y-%m-%d"),format = "%j")   
  airtemp.long$year_doy <- paste(airtemp.long$year,airtemp.long$doy, sep="") 
  airtemp<-tapply(airtemp.long$airtemp,list(airtemp.long$year_doy,airtemp.long$plot), mean, na.rm=T)#mean daily air
  airtemp<-as.data.frame(cbind(substr(rownames(airtemp),1,4),substr(rownames(airtemp),5,7),airtemp))
  colnames(airtemp)[1:2]<-c("year","doy")
  airtemp2<-reshape(airtemp,varying = list(colnames(airtemp)[3:26]), direction = "long", v.names = c("airtemp"), times = c(colnames(airtemp)[3:26]))
  colnames(airtemp2)[3]<-"plot"
  allclim<-merge(airtemp2,soiltemp2,by=c("year","doy","plot","id"), all.y=TRUE)  
  allclim2<-merge(allclim,mois2,by=c("year","doy","plot","id"), all=TRUE) 
  allclim2$site<-"clarkduke"
  allclim2$treatment<-NA
  allclim2[allclim2$plot=="G01"|allclim2$plot=="G04"|allclim2$plot=="G07"|allclim2$plot=="S03"|allclim2$plot=="S04"|allclim2$plot=="S08",]$treatment<-"ambient"
  allclim2[allclim2$plot=="G02"|allclim2$plot=="G06"|allclim2$plot=="G08"|allclim2$plot=="S01"|allclim2$plot=="S06"|allclim2$plot=="S07",]$treatment<-"warm5C"
  allclim2[allclim2$plot=="G03"|allclim2$plot=="G05"|allclim2$plot=="G09"|allclim2$plot=="S02"|allclim2$plot=="S05"|allclim2$plot=="S09",]$treatment<-"warm3C"
  allclim2[allclim2$plot=="G10"|allclim2$plot=="G11"|allclim2$plot=="G12"|allclim2$plot=="S10"|allclim2$plot=="S11"|allclim2$plot=="S12",]$treatment<-"warm3C"
  clarkdukeclim<-subset(allclim2, select=c("site","treatment","plot","year","doy","airtemp","soiltemp","soilmois"))
  row.names(clarkdukeclim) <- NULL
  return(clarkdukeclim) 
}

##Produce cleaned, raw climate data
raw.data.dir <- "./Experiments/"
cleanclimdata.raw <- list()
cleanclimdata.raw$marchin <- clean.clim$marchin(path="./Experiments/marchin")
cleanclimdata.raw$farnsworth <- clean.clim$farnsworth(path="./Experiments/farnsworth/")
cleanclimdata.raw$clarkharvard <- clean.clim$clarkharvard(path="./Experiments/clark/")
cleanclimdata.raw$clarkduke <- clean.clim$clarkduke(path="./Experiments/clark/")

head(cleanclimdata.raw$farnsworth)
expphenclim <- do.call("rbind", cleanclimdata.raw)
row.names(expphenclim) <- NULL
write.csv(expphenclim, "radmeeting/expclim.csv", row.names=FALSE)
