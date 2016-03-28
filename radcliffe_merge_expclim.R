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
  ##climate data available at: http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf113
  file <- file.path(path, filename)
  marchin1 <- read.csv(file, check.names=FALSE, header=TRUE)
  names(marchin1)[8]<-"plot"
  marchin1$year_doy<-paste(marchin1$year,marchin1$doy, sep="-")
  #get min airtemp across all 3 measurements for each plot
  temp_min<-aggregate(x=subset(marchin1, select=c("CAT1_Min","CAT2_Min","CAT3_Min","CSTo1_Min","CSTo2_Min","CSTI1_Min","CSTI2_Min")), by=list(marchin1$year_doy,marchin1$plot), FUN=min)
  airtemp_min<-apply(temp_min[,3:5],1,min)
  soiltemp1_min<-apply(temp_min[,6:7],1,min)#temp at 2cm depth(organic)
  soiltemp2_min<-apply(temp_min[,8:9],1,min)#temp at 6cm depth(inorganic)
  temp_max<-aggregate(x=subset(marchin1, select=c("CAT1_Max","CAT2_Max","CAT3_Max","CSTo1_Max","CSTo2_Max","CSTI1_Max","CSTI2_Max")), by=list(marchin1$year_doy,marchin1$plot), FUN=max)
  airtemp_max<-apply(temp_max[,3:5],1,max)
  soiltemp1_max<-apply(temp_max[,6:7],1,max)#temp at 2cm depth(organic)
  soiltemp2_max<-apply(temp_max[,8:9],1,max)#temp at 6cm depth(inorganic)
  soilmois<-aggregate(x=subset(marchin1, select=c("CSM_Avg")), by=list(marchin1$year_doy,marchin1$plot), FUN=mean)
  colnames(temp_min)[1:2]<-c("year_doy","plot")
  year_doy <- strsplit(temp_min$year_doy,'-') 
  year_doy<-do.call(rbind, year_doy)
  allclim<-as.data.frame(cbind(year_doy,airtemp_min,airtemp_max,soiltemp1_min,soiltemp2_min,soiltemp1_max,soiltemp2_max,soilmois))
  colnames(allclim)[9:11]<-c("year_doy","plot","soilmois")
  colnames(allclim)[1:2]<-c("year","doy")
  allclim$preciptreat<-NA
  allclim$temptreat<-1
  allclim[allclim$plot==2,]$temptreat<-0
  allclim[allclim$plot==5,]$temptreat<-0
  allclim[allclim$plot==11,]$temptreat<-0
  allclim1<-subset(allclim, select=c("temptreat","preciptreat","plot","year","doy","airtemp_min","airtemp_max","soiltemp1_min","soiltemp2_min","soiltemp1_max","soiltemp2_max","soilmois"))
  file2<-file.path(path, "hf113-11-df-outside.csv")
  marchin2<-read.csv(file2, header=TRUE)
  marchin2$year_doy<-paste(marchin2$year,marchin2$doy, sep="-")
  marchin2$plot<-"outside"
  temp_min2<-aggregate(x=subset(marchin2, select=c("oAT1_Min","oAT2_Min","oAT3_Min","oSTo1_Min","oSTo2_Min","oSTo3_Min","oSTI1_Min","oSTI2_Min","oSTI3_Min")), by=list(marchin2$year_doy,marchin2$plot), FUN=min)
  airtemp_min<-apply(temp_min2[,3:5],1,min)
  soiltemp1_min<-apply(temp_min2[,6:8],1,min)#temp at 2cm depth(organic)
  soiltemp2_min<-apply(temp_min2[,9:11],1,min)#temp at 6cm depth(inorganic)
  temp_max2<-aggregate(x=subset(marchin2, select=c("oAT1_Max","oAT2_Max","oAT3_Max","oSTo1_Max","oSTo2_Max","oSTo3_Max","oSTI1_Max","oSTI2_Max","oSTI3_Max")), by=list(marchin2$year_doy,marchin2$plot), FUN=max)
  airtemp_max<-apply(temp_max2[,3:5],1,max)
  soiltemp1_max<-apply(temp_max2[,6:8],1,max)#temp at 2cm depth(organic)
  soiltemp2_max<-apply(temp_max2[,9:11],1,max)#temp at 6cm depth(inorganic)
  #looks like no soil moisture for outside chambers?
  colnames(temp_min2)[1:2]<-c("year_doy","plot")
  year_doy <- strsplit(temp_min2$year_doy,'-') 
  year_doy<-do.call(rbind, year_doy)
  temptreat<-rep("outside",times=dim(year_doy)[1])
  preciptreat<-rep("outside",times=dim(year_doy)[1])
oallclim<-as.data.frame(cbind(temptreat,preciptreat,temp_min2$plot,year_doy,airtemp_min, airtemp_max,soiltemp1_min, soiltemp2_min,soiltemp1_max,soiltemp2_max))
oallclim$soilmois<-NA
colnames(oallclim)[3:5]<-c("plot","year","doy")
allclim2<-rbind(allclim1,oallclim)
allclim2$site<-"marchin"
allclim2$soiltemp1_mean<-(as.numeric(allclim2$soiltemp1_min)+as.numeric(allclim2$soiltemp1_max))/2
marchinclim<-subset(allclim2, select=c("site","temptreat","preciptreat","plot","year","doy","airtemp_min","airtemp_max","soiltemp1_min","soiltemp2_min","soiltemp1_max","soiltemp2_max","soiltemp1_mean","soilmois"))
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
  temp.long<-reshape(temp,varying = list(colnames(temp)[6:23]), direction = "long", v.names = c("soiltemp1_mean"), times = c(colnames(temp)[6:23]))
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
  temp.long2<-subset(temp.long,selec=c("year","doy","plot","soiltemp1_mean"))
  file2<-file.path(path, "hf005-05-soil-respiration.csv")
  mois<-read.csv(file2,  header=TRUE)
  mois$doy<-strftime(strptime(paste(mois$year,mois$month,mois$day,sep="-"), format = "%Y-%m-%d"),format = "%j") 
  mois2<-subset(mois,selec=c("year","doy","plot","treatment","moisture","temp.4cm"))
  allclim<-merge(temp.long2,mois2,by.x=c("year","doy","plot"),by.y=c("year","doy","plot"),all=TRUE)
  allclim$airtemp_min<-NA 
  allclim$airtemp_max<-NA 
  allclim$soiltemp1_min<-NA
  allclim$soiltemp2_min<-NA
  allclim$soiltemp1_max<-NA
  allclim$soiltemp2_max<-NA
  colnames(allclim)[6]<-"soilmois"
  colnames(allclim)[5]<-"treatment2"
  allclim$temptreat<-NA
  allclim[which(allclim$treatment2=="H"),]$temptreat<-1
  allclim[which(allclim$treatment2=="C"),]$temptreat<-0
  allclim[which(allclim$treatment2=="DC"),]$temptreat<-"sham"
  allclim$site<-"farnsworth"
  allclim$preciptreat<-NA
  farnsworthclim<-subset(allclim, select=c("site","temptreat","preciptreat","plot","year","doy","airtemp_min","airtemp_max","soiltemp1_min","soiltemp2_min","soiltemp1_max","soiltemp2_max","soiltemp1_mean","soilmois"))
  row.names(farnsworthclim) <- NULL
  return(farnsworthclim) 
}

##Clark et al from Harvard ##
## Data type: hourly air temp, soil temp (celsius, at 5cm depth) and soil moisture (Estimated ratio of volumetric water content, scaled from 0 to 1) in heated plots, "ambient" plots = like disturbance control plots, and control plots with mesh walls, no heating infrastructure
## Notes: Public data, 
clean.clim$clarkharvard <- function(filename="data-Harvard-ST1.csv", path="./Experiments/clark") {
  file <- file.path(path, filename)
  soiltemp_hr<- read.csv(file, header=TRUE)
  soiltemp.long<-reshape(soiltemp_hr,varying = list(colnames(soiltemp_hr)[8:31]), direction = "long", v.names = c("soiltemp"), times = c(colnames(soiltemp_hr)[8:31]))
  soiltemp.long$plot<-substr(soiltemp.long$time,1,3)
  soiltemp.long$doy<-strftime(strptime(paste(soiltemp.long$year,soiltemp.long$month,soiltemp.long$day,sep="-"), format = "%Y-%m-%d"),format = "%j")
  soiltemp.long$year_doy <- paste(soiltemp.long$year,soiltemp.long$doy, sep="") 
  soiltemp1_min<-aggregate(x=subset(soiltemp.long, select="soiltemp"), by=list(soiltemp.long$year_doy,soiltemp.long$plot), FUN=min)
  colnames(soiltemp1_min)<-c("year_doy","plot","soiltemp1_min")
  soiltemp1_max<-aggregate(x=subset(soiltemp.long, select="soiltemp"), by=list(soiltemp.long$year_doy,soiltemp.long$plot), FUN=max)
  colnames(soiltemp1_max)<-c("year_doy","plot","soiltemp1_max")
  soiltemp<-as.data.frame(cbind(soiltemp1_min$year_doy,soiltemp1_min$plot,soiltemp1_min$soiltemp1_min,soiltemp1_max$soiltemp1_max))
  colnames(soiltemp)<-c("year_doy","plot","soiltemp1_min","soiltemp1_max")
  
  file2<-file.path(path, "data-Harvard-SM.csv")
  mois_hr<-read.csv(file2,  header=TRUE)
  mois.long<-reshape(mois_hr,varying = list(colnames(mois_hr)[8:31]), direction = "long", v.names = c("soilmois"), times = c(colnames(mois_hr)[8:31]))
  mois.long$plot<-substr(mois.long$time,1,3)
  mois.long$doy<-strftime(strptime(paste(mois.long$year,mois.long$month,mois.long$day,sep="-"), format = "%Y-%m-%d"),format = "%j")   
  mois.long$year_doy <- paste(mois.long$year,mois.long$doy, sep="") 
  soilmois<-aggregate(x=subset(mois.long, select="soilmois"), by=list(soiltemp.long$year_doy,soiltemp.long$plot), FUN=mean)
  colnames(soilmois)<-c("year_doy","plot","soilmois")
  
  file3<-file.path(path, "data-Harvard-AT.csv")
  airtemp_hr<-read.csv(file3,  header=TRUE)
  airtemp.long<-reshape(airtemp_hr,varying = list(colnames(airtemp_hr)[8:31]), direction = "long", v.names = c("airtemp"), times = c(colnames(airtemp_hr)[8:31]))
  airtemp.long$plot<-substr(airtemp.long$time,1,3)
  airtemp.long$doy<-strftime(strptime(paste(airtemp.long$year,airtemp.long$month,airtemp.long$day,sep="-"), format = "%Y-%m-%d"),format = "%j")   
  airtemp.long$year_doy <- paste(airtemp.long$year,airtemp.long$doy, sep="") 
  airtemp_min<-aggregate(x=subset(airtemp.long, select="airtemp"), by=list(airtemp.long$year_doy,airtemp.long$plot), FUN=min)
  colnames(airtemp_min)<-c("year_doy","plot","airtemp_min")
  airtemp_max<-aggregate(x=subset(airtemp.long, select="airtemp"), by=list(airtemp.long$year_doy,airtemp.long$plot), FUN=max)
  colnames(airtemp_max)<-c("year_doy","plot","airtemp_max")
  airtemp<-as.data.frame(cbind(airtemp_min$year_doy,airtemp_min$plot,airtemp_min$airtemp_min,airtemp_max$airtemp_max))
  colnames(airtemp)<-c("year_doy","plot","airtemp_min","airtemp_max")
  
  allclim<-merge(airtemp,soiltemp,by=c("year_doy","plot"), all=TRUE)  
  allclim2<-merge(allclim,soilmois,by=c("year_doy","plot"), all=TRUE) 
allclim2$year<-substr(allclim2$year_doy,1,4)
allclim2$doy<-substr(allclim2$year_doy,5,7)  
allclim2$site<-"clarkharvard"
  allclim2$temptreat<-NA
  allclim2[allclim2$plot=="G02"|allclim2$plot=="G04"|allclim2$plot=="G07"|allclim2$plot=="S02"|allclim2$plot=="S05"|allclim2$plot=="S07",]$temptreat<-"sham"#disturbance control
  allclim2[allclim2$plot=="G03"|allclim2$plot=="G05"|allclim2$plot=="G09"|allclim2$plot=="S01"|allclim2$plot=="S04"|allclim2$plot=="S09",]$temptreat<-2
  allclim2[allclim2$plot=="G01"|allclim2$plot=="G06"|allclim2$plot=="G08"|allclim2$plot=="S03"|allclim2$plot=="S06"|allclim2$plot=="S08",]$temptreat<-1
  allclim2[allclim2$plot=="G10"|allclim2$plot=="G11"|allclim2$plot=="G12"|allclim2$plot=="S10"|allclim2$plot=="S11"|allclim2$plot=="S12",]$temptreat<-0
  allclim2$preciptreat<-NA
  allclim2$soiltemp2_min<-NA
  allclim2$soiltemp2_max<-NA
  allclim2$soiltemp1_mean<-(as.numeric(allclim2$soiltemp1_min)+as.numeric(allclim2$soiltemp1_max))/2
clarkharvardclim<-subset(allclim2, select=c("site","temptreat","preciptreat","plot","year","doy","airtemp_min","airtemp_max","soiltemp1_min","soiltemp2_min","soiltemp1_max","soiltemp2_max","soiltemp1_mean","soilmois"))
  row.names(clarkharvardclim) <- NULL
  return(clarkharvardclim) 
}
##Clark et al from Duke ##
## Data type: hourly air temp, soil temp (celsius, at 5cm depth) and soil moisture (Estimated ratio of volumetric water content, scaled from 0 to 1) in heated plots, "ambient" plots = like disturbance control plots, and control plots with mesh walls, no heating infrastructure
## Notes: Public data, 
clean.clim$clarkduke <- function(filename="data-Duke-ST.csv", path="./Experiments/clark") {
  file <- file.path(path, filename)
  soiltemp_hr<- read.csv(file, header=TRUE)
  soiltemp.long<-reshape(soiltemp_hr,varying = list(colnames(soiltemp_hr)[8:31]), direction = "long", v.names = c("soiltemp"), times = c(colnames(soiltemp_hr)[8:31]))
  soiltemp.long$plot<-substr(soiltemp.long$time,1,3)
  soiltemp.long$doy<-strftime(strptime(paste(soiltemp.long$year,soiltemp.long$month,soiltemp.long$day,sep="-"), format = "%Y-%m-%d"),format = "%j")
  soiltemp.long$year_doy <- paste(soiltemp.long$year,soiltemp.long$doy, sep="") 
  soiltemp1_min<-aggregate(x=subset(soiltemp.long, select="soiltemp"), by=list(soiltemp.long$year_doy,soiltemp.long$plot), FUN=min)
  colnames(soiltemp1_min)<-c("year_doy","plot","soiltemp1_min")
  soiltemp1_max<-aggregate(x=subset(soiltemp.long, select="soiltemp"), by=list(soiltemp.long$year_doy,soiltemp.long$plot), FUN=max)
  colnames(soiltemp1_max)<-c("year_doy","plot","soiltemp1_max")
  soiltemp<-as.data.frame(cbind(soiltemp1_min$year_doy,soiltemp1_min$plot,soiltemp1_min$soiltemp1_min,soiltemp1_max$soiltemp1_max))
  colnames(soiltemp)<-c("year_doy","plot","soiltemp1_min","soiltemp1_max")
  
  file2<-file.path(path, "data-Duke-SM.csv")
  mois_hr<-read.csv(file2,  header=TRUE)
  mois.long<-reshape(mois_hr,varying = list(colnames(mois_hr)[8:31]), direction = "long", v.names = c("soilmois"), times = c(colnames(mois_hr)[8:31]))
  mois.long$plot<-substr(mois.long$time,1,3)
  mois.long$doy<-strftime(strptime(paste(mois.long$year,mois.long$month,mois.long$day,sep="-"), format = "%Y-%m-%d"),format = "%j")   
  mois.long$year_doy <- paste(mois.long$year,mois.long$doy, sep="") 
  soilmois<-aggregate(x=subset(mois.long, select="soilmois"), by=list(soiltemp.long$year_doy,soiltemp.long$plot), FUN=mean)
  colnames(soilmois)<-c("year_doy","plot","soilmois")
  
  file3<-file.path(path, "data-Duke-AT.csv")
  airtemp_hr<-read.csv(file3,  header=TRUE)
  airtemp.long<-reshape(airtemp_hr,varying = list(colnames(airtemp_hr)[8:31]), direction = "long", v.names = c("airtemp"), times = c(colnames(airtemp_hr)[8:31]))
  airtemp.long$plot<-substr(airtemp.long$time,1,3)
  airtemp.long$doy<-strftime(strptime(paste(airtemp.long$year,airtemp.long$month,airtemp.long$day,sep="-"), format = "%Y-%m-%d"),format = "%j")   
  airtemp.long$year_doy <- paste(airtemp.long$year,airtemp.long$doy, sep="") 
  airtemp_min<-aggregate(x=subset(airtemp.long, select="airtemp"), by=list(airtemp.long$year_doy,airtemp.long$plot), FUN=min)
  colnames(airtemp_min)<-c("year_doy","plot","airtemp_min")
  airtemp_max<-aggregate(x=subset(airtemp.long, select="airtemp"), by=list(airtemp.long$year_doy,airtemp.long$plot), FUN=max)
  colnames(airtemp_max)<-c("year_doy","plot","airtemp_max")
  airtemp<-as.data.frame(cbind(airtemp_min$year_doy,airtemp_min$plot,airtemp_min$airtemp_min,airtemp_max$airtemp_max))
  colnames(airtemp)<-c("year_doy","plot","airtemp_min","airtemp_max")
  
  allclim<-merge(airtemp,soiltemp,by=c("year_doy","plot"), all=TRUE)  
  allclim2<-merge(allclim,soilmois,by=c("year_doy","plot"), all=TRUE) 
  allclim2$year<-substr(allclim2$year_doy,1,4)
  allclim2$doy<-substr(allclim2$year_doy,5,7)
  allclim2$site<-"clarkduke"
  allclim2$temptreat<-NA
  allclim2[allclim2$plot=="G01"|allclim2$plot=="G04"|allclim2$plot=="G07"|allclim2$plot=="S03"|allclim2$plot=="S04"|allclim2$plot=="S08",]$temptreat<-"sham"#disturbance control
  allclim2[allclim2$plot=="G02"|allclim2$plot=="G06"|allclim2$plot=="G08"|allclim2$plot=="S01"|allclim2$plot=="S06"|allclim2$plot=="S07",]$temptreat<-2
  allclim2[allclim2$plot=="G03"|allclim2$plot=="G05"|allclim2$plot=="G09"|allclim2$plot=="S02"|allclim2$plot=="S05"|allclim2$plot=="S09",]$temptreat<-1
  allclim2[allclim2$plot=="G10"|allclim2$plot=="G11"|allclim2$plot=="G12"|allclim2$plot=="S10"|allclim2$plot=="S11"|allclim2$plot=="S12",]$temptreat<-0
  allclim2$preciptreat<-NA
  allclim2$preciptreat<-NA
  allclim2$soiltemp2_min<-NA
  allclim2$soiltemp2_max<-NA
  allclim2$soiltemp1_mean<-(as.numeric(allclim2$soiltemp1_min)+as.numeric(allclim2$soiltemp1_max))/2
  clarkdukeclim<-subset(allclim2, select=c("site","temptreat","preciptreat","plot","year","doy","airtemp_min","airtemp_max","soiltemp1_min","soiltemp2_min","soiltemp1_max","soiltemp2_max","soiltemp1_mean","soilmois"))
  row.names(clarkdukeclim) <- NULL
  return(clarkdukeclim) 
}

##Dukes et al. data from BACE ##
## Data type: hourly soil temp, from 2 and 10 cm; soil mois=VWC, just using 2010 data sine that is when we have phenology
## Notes: data shared by Jeff Dukes (jsdukes@purdue.edu)
clean.clim$bace <- function(filename="2010SoilTempDaily.csv", path="./Experiments/bace") {
file <- file.path(path, filename)
soiltemp<- read.csv(file, header=TRUE,na.strings = ".")
colnames(soiltemp) [3:5]<-c("doy","plot","temptreat")
colnames(soiltemp) [9:12]<-c("soiltemp1_min","soiltemp2_min","soiltemp1_max","soiltemp2_max")
soiltemp$preciptreat<-NA
soiltemp[soiltemp$precip.treatment==1,]$preciptreat<-0#ambient precip
soiltemp[soiltemp$precip.treatment==0,]$preciptreat<--1#50% precip
soiltemp[soiltemp$precip.treatment==2,]$preciptreat<-1#150% ambient precip
file2 <- file.path(path, "bace_soilmoisture2010.csv")
soilmois<-read.csv(file2, header=T,na.strings = ".")
colnames(soilmois)[3]<-"doy"
allclim<-merge(soiltemp,soilmois,by=c("year","doy","plot"),all=TRUE) 
colnames(allclim)[17]<-"soilmois"
allclim[which(allclim$soilmois==209),]$soilmois<-NA#remove weird values for soil moisture, which should be between 0 and 1 (209, 1.87)
allclim[which(allclim$soilmois==1.87),]$soilmois<-NA
allclim$airtemp_min<-NA
allclim$airtemp_max<-NA
allclim$site<-"bace"
allclim$soiltemp1_mean<-(as.numeric(allclim$soiltemp1_min)+as.numeric(allclim$soiltemp1_max))/2
baceclim<-subset(allclim, select=c("site","temptreat","preciptreat","plot","year","doy","airtemp_min","airtemp_max","soiltemp1_min","soiltemp2_min","soiltemp1_max","soiltemp2_max","soiltemp1_mean","soilmois"))
row.names(baceclim) <- NULL
return(baceclim) 
}
##Climate data for Ellison from Harvard Forest##
## Data type: hourly soil temp, from 2 and 10 cm; soil mois=VWC, just using 2010 data sine that is when we have phenology
## Notes: http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf113; i had to subset this datafile before pushing it to github as it was too large. i selected out the columns that we wanted and removed 2009 (not included in phenology data), but otherwise left data untouched
clean.clim$ellison <- function(filename="ellison_subsetclim.csv", path="./Experiments/ellison") {
  file <- file.path(path, filename)
  ellison1 <- read.csv(file, check.names=FALSE, header=TRUE)
  names(ellison1)[9]<-"plot"
  ellison1$year_doy<-paste(ellison1$year,ellison1$doy, sep="-")
  #get min airtemp across both measurements for each plot
  temp_min<-aggregate(x=subset(ellison1, select=c("cat1.min","cat2.min","csto1.min","csto2.min","csti1.min","csti2.min")), by=list(ellison1$year_doy,ellison1$plot), FUN=min)
  airtemp_min<-apply(temp_min[,3:4],1,min)
  soiltemp1_min<-apply(temp_min[,5:6],1,min)#temp at 2cm depth(organic)
  soiltemp2_min<-apply(temp_min[,7:8],1,min)#temp at 6cm depth(inorganic)
  temp_max<-aggregate(x=subset(ellison1, select=c("cat1.max","cat2.max","csto1.max","csto2.max","csti1.max","csti2.max")), by=list(ellison1$year_doy,ellison1$plot), FUN=max)
  airtemp_max<-apply(temp_max[,3:4],1,max)
  soiltemp1_max<-apply(temp_max[,5:6],1,max)#temp at 2cm depth(organic)
  soiltemp2_max<-apply(temp_max[,7:8],1,max)#temp at 6cm depth(inorganic)
  soilmois<-aggregate(x=subset(ellison1, select=c("csm.avg")), by=list(ellison1$year_doy,ellison1$plot), FUN=mean)
  colnames(temp_min)[1:2]<-c("year_doy","plot")
  year_doy <- strsplit(temp_min$year_doy,'-') 
  year_doy<-do.call(rbind, year_doy)
  allclim<-as.data.frame(cbind(year_doy,airtemp_min,airtemp_max,soiltemp1_min,soiltemp2_min,soiltemp1_max,soiltemp2_max,soilmois))
  colnames(allclim)[9:11]<-c("year_doy","plot","soilmois")
  colnames(allclim)[1:2]<-c("year","doy")
  allclim$preciptreat<-NA
  allclim$temptreat<-NA
  allclim[allclim$plot==6,]$temptreat<-0
  allclim[allclim$plot==4,]$temptreat<-0
  allclim[allclim$plot==11,]$temptreat<-0
  allclim[allclim$plot==7,]$temptreat<-4
  allclim[allclim$plot==8,]$temptreat<-1
  allclim[allclim$plot==9,]$temptreat<-3
  allclim[allclim$plot==10,]$temptreat<-7
  allclim[allclim$plot==12,]$temptreat<-5
  allclim[allclim$plot==1,]$temptreat<-9
  allclim[allclim$plot==2,]$temptreat<-6
  allclim[allclim$plot==3,]$temptreat<-2
  allclim[allclim$plot==5,]$temptreat<-8
  allclim1<-subset(allclim, select=c("temptreat","preciptreat","plot","year","doy","airtemp_min","airtemp_max","soiltemp1_min","soiltemp2_min","soiltemp1_max","soiltemp2_max","soilmois"))
  file2<-file.path(path, "hf113-03-hf-outside.csv")
  ellison2<-read.csv(file2, header=TRUE)
  ellison2<-ellison2[-which(ellison2$year==2009),]
  ellison2$year_doy<-paste(ellison2$year,ellison2$doy, sep="-")
  ellison2$plot<-"outside"
  temp_min2<-aggregate(x=subset(ellison2, select=c("oat1.min","oat2.min","oat3.min","osto1.min","osto2.min","osto3.min","osti1.min","osti2.min","osti3.min")), by=list(ellison2$year_doy,ellison2$plot), FUN=min)
  airtemp_min<-apply(temp_min2[,3:5],1,min)
  soiltemp1_min<-apply(temp_min2[,6:8],1,min)#temp at 2cm depth(organic)
  soiltemp2_min<-apply(temp_min2[,9:11],1,min)#temp at 6cm depth(inorganic)
  temp_max2<-aggregate(x=subset(ellison2, select=c("oat1.max","oat2.max","oat3.max","osto1.max","osto2.max","osto3.max","osti1.max","osti2.max","osti3.max")), by=list(ellison2$year_doy,ellison2$plot), FUN=max)
  airtemp_max<-apply(temp_max2[,3:5],1,max)
  soiltemp1_max<-apply(temp_max2[,6:8],1,max)#temp at 2cm depth(organic)
  soiltemp2_max<-apply(temp_max2[,9:11],1,max)#temp at 6cm depth(inorganic)
  soilmois2<-aggregate(x=subset(ellison2, select=c("oc1sm.avg","oc2sm.avg","oc3sm.avg")), by=list(ellison2$year_doy,ellison2$plot), FUN=mean)
  soilmois<-apply(soilmois2[,3:5],1,mean)#temp at 6cm depth(inorganic)
  colnames(temp_min2)[1:2]<-c("year_doy","plot")
  year_doy <- strsplit(temp_min2$year_doy,'-') 
  year_doy<-do.call(rbind, year_doy)
  temptreat<-rep("outside",times=dim(year_doy)[1])
  preciptreat<-rep("outside",times=dim(year_doy)[1])
  oallclim<-as.data.frame(cbind(temptreat,preciptreat,temp_min2$plot,year_doy,airtemp_min, airtemp_max,soiltemp1_min, soiltemp2_min,soiltemp1_max,soiltemp2_max,soilmois))
  colnames(oallclim)[3:5]<-c("plot","year","doy")
  allclim2<-rbind(allclim1,oallclim)
  allclim2$site<-"ellison"
  allclim2$soiltemp1_mean<-(as.numeric(allclim2$soiltemp1_min)+as.numeric(allclim2$soiltemp1_max))/2
  ellisonclim<-subset(allclim2, select=c("site","temptreat","preciptreat","plot","year","doy","airtemp_min","airtemp_max","soiltemp1_min","soiltemp2_min","soiltemp1_max","soiltemp2_max","soiltemp1_mean","soilmois"))
  row.names(ellisonclim) <- NULL
  return(ellisonclim)
}

##
clean.clim$sherryok<- function(filename="IRCEBprojectSoilMoist20032004.csv", path="./Experiments/sherry") {
  file <- file.path(path, filename)
  soilmois<- read.csv(file, skip=2,na.strings = "-99999",header=TRUE)
  site<-"sherry"
  soilmois$year<-paste("20",substr(soilmois$Date,(nchar(soilmois$Date)+1)-2,nchar(soilmois$Date)), sep="")
  colnames(soilmois)[2]<-"doy"
  colnames(soilmois)[3]<-"soilmois"
  soilmois.control<-rbind(soilmois[,1:3],soilmois[,1:3],soilmois[,1:3],soilmois[,1:3],soilmois[,1:3])
  soilmois.control$plot<-c(rep(2,times=dim(soilmois)[1]),rep(6,times=dim(soilmois)[1]),rep(10,times=dim(soilmois)[1]),rep(14,times=dim(soilmois)[1]),rep(18,times=dim(soilmois)[1]))
  soilmois.control$temptreat<-0
  soilmois.control$preciptreat<-0
  soilmois.water<-rbind(cbind(soilmois[,1:2],soilmois$W1),cbind(soilmois[,1:2],soilmois$W1),cbind(soilmois[,1:2],soilmois$W1),cbind(soilmois[,1:2],soilmois$W1),cbind(soilmois[,1:2],soilmois$W1))
  soilmois.water$plot<-c(rep(4,times=dim(soilmois)[1]),rep(8,times=dim(soilmois)[1]),rep(11,times=dim(soilmois)[1]),rep(16,times=dim(soilmois)[1]),rep(20,times=dim(soilmois)[1]))
  colnames(soilmois.water)[3]<-"soilmois"
  soilmois.water$temptreat<-0
  soilmois.water$preciptreat<-1
  soilmois.heat<-rbind(cbind(soilmois[,1:2],soilmois$H1),cbind(soilmois[,1:2],soilmois$H1),cbind(soilmois[,1:2],soilmois$H1),cbind(soilmois[,1:2],soilmois$H1),cbind(soilmois[,1:2],soilmois$H1))
  soilmois.heat$plot<-c(rep(3,times=dim(soilmois)[1]),rep(7,times=dim(soilmois)[1]),rep(12,times=dim(soilmois)[1]),rep(15,times=dim(soilmois)[1]),rep(19,times=dim(soilmois)[1]))
  soilmois.heat$temptreat<-1
  soilmois.heat$preciptreat<-0
  colnames(soilmois.heat)[3]<-"soilmois"
  soilmois.both<-rbind(cbind(soilmois[,1:2],soilmois$B1),cbind(soilmois[,1:2],soilmois$B1),cbind(soilmois[,1:2],soilmois$B1),cbind(soilmois[,1:2],soilmois$B1),cbind(soilmois[,1:2],soilmois$B1))
  soilmois.both$plot<-c(rep(5,times=dim(soilmois)[1]),rep(9,times=dim(soilmois)[1]),rep(13,times=dim(soilmois)[1]),rep(17,times=dim(soilmois)[1]),rep(1,times=dim(soilmois)[1]))
  soilmois.both$temptreat<-1
  soilmois.both$preciptreat<-1
  colnames(soilmois.both)[3]<-"soilmois"
  soilmois.all<-rbind(soilmois.control,soilmois.water,soilmois.heat,soilmois.both)
  soilmois.all$year<-NA
  soilmois.all$year<-paste("20",substr(soilmois.all$Date,(nchar(soilmois.all$Date)+1)-2,nchar(soilmois.all$Date)), sep="")
  sherrytempfiles<-c("IRCEB2003JD1_35.csv","IRCEB2003JD36_69.csv","IRCEB2003JD69_103.csv","IRCEB2003JD104_205.csv","IRCEB2003JD206_239.csv","IRCEB2003JD240_273.csv","IRCEB2003JD274_303.csv","IRCEB2003JD308_340.csv","IRCEB2003JD341_365.csv","IRCEB2004JD1_102.csv","IRCEB2004JD103_170.csv","IRCEB2004JD171_204.csv","IRCEB2004JD205_238.csv","IRCEB2004JD239_249.csv","IRCEB2004JD287_320.csv","IRCEB2004JD321_323.csv","IRCEB2004JD326_359.csv","IRCEB2004JD360_366.csv")
  alltemp<-NA
  for (i in 1:length(sherrytempfiles)){
    file <- file.path(path, paste(sherrytempfiles[i]))
    temp <- read.csv(file, header=TRUE,na.strings = "-99999")
    if(i<4){colnames(temp)[4]<-"Air.15.cm"}
    if(i<4){colnames(temp)[5]<-"X7.5cm"}
    temp2<-temp[which(temp$Plot>=1),]
    minairtemp<-aggregate(x=temp2$Air.15.cm, by=list(temp2$J,temp2$Plot), FUN=min)
    colnames(minairtemp)<-c("doy","plot","airtemp_min")
    maxairtemp<-aggregate(x=temp2$Air.15.cm, by=list(temp2$J,temp2$Plot), FUN=max)
    minsoiltemp<-aggregate(x=temp2$X7.5cm, by=list(temp2$J,temp2$Plot), FUN=min)
    maxsoiltemp<-aggregate(x=temp2$X7.5cm, by=list(temp2$J,temp2$Plot), FUN=max)
    temps<-cbind(minairtemp, as.numeric(maxairtemp[,3]),as.numeric(minsoiltemp[,3]),as.numeric(maxsoiltemp[,3]))
    temps$year<-substr(sherrytempfiles[i],6,9)
    alltemp<-rbind(alltemp,temps)
  }
  colnames(alltemp)<-c("doy","plot","airtemp_min","airtemp_max","soiltemp1_min","soiltemp1_max","year") 
  alltemp<-alltemp[-1,]
  allclim<-merge(alltemp,soilmois.all, all=TRUE)
  allclim$soiltemp2_min<-NA
  allclim$soiltemp2_max<-NA
  allclim$soiltemp1_mean<-(allclim$soiltemp1_max+allclim$soiltemp1_min)/2
  allclim$site<-sherry
  sherryclim<-subset(allclim, select=c("site","temptreat","preciptreat","plot","year","doy","airtemp_min","airtemp_max","soiltemp1_min","soiltemp2_min","soiltemp1_max","soiltemp2_max","soiltemp1_mean","soilmois"))
  row.names(sherryclim) <- NULL
  return(sherryclim)
}

##Chuine et al. data from ??? ##
## Data type: sporadic measurements of soil temp and humidity?
## Notes: data shared by isabelle chuine (isabelle.chuine@cefe.cnrs.fr)
clean.clim$chuine <- function(path="./Experiments/chuine") {
  soilfiles<-c("TDR2002.csv","TDR2003.csv","TDR2004.csv","TDR2005.csv")
  for (i in 1:length(soilfiles)){
    file <- file.path(path, paste(maxtempfiles[i]))
    soiltemp<- read.csv(file, header=TRUE,na.strings = ".")

    
##Produce cleaned, raw climate data
raw.data.dir <- "./Experiments/"
cleanclimdata.raw <- list()
cleanclimdata.raw$marchin <- clean.clim$marchin(path="./Experiments/marchin")
cleanclimdata.raw$farnsworth <- clean.clim$farnsworth(path="./Experiments/farnsworth/")
cleanclimdata.raw$clarkharvard <- clean.clim$clarkharvard(path="./Experiments/clark/")
cleanclimdata.raw$clarkduke <- clean.clim$clarkduke(path="./Experiments/clark/")
cleanclimdata.raw$bace <- clean.clim$bace(path="./Experiments/bace")
cleanclimdata.raw$ellison <- clean.clim$ellison(path="./Experiments/ellison")
cleanclimdata.raw$sherry <- clean.clim$sherry(path="./Experiments/sherry")

#cleanclimdata.raw$chuine <- clean.clim$chuine(path="./Experiments/chuine")
#cleanclimdata.raw$jasper <- clean.clim$jasper(path="./Experiments/jasper")


expphenclim1 <- do.call("rbind", cleanclimdata.raw)
row.names(expphenclim1) <- NULL

expphenclim<-expphenclim1[-which(expphenclim1$doy=="NA"),]
expphenclim$doy<-as.numeric(expphenclim$doy)
###Add GDD to all: soiltemp-tbase (=10), cumulative GDD for that year (sum up to that date)
tbase<-10
expphenclim$gdd_soil<-expphenclim$soiltemp1_mean-tbase
expphenclim$gdd_air<-((as.numeric(expphenclim$airtemp_min)+as.numeric(expphenclim$airtemp_max))/2)-tbase
expphenclim$gdd_soil[expphenclim$gdd_soil < 10] <- 0
expphenclim$gdd_air[expphenclim$gdd_air < 10] <- 0
expphenclim<-expphenclim[order(expphenclim$site,expphenclim$plot,expphenclim$year, expphenclim$doy),]
expphenclim$cumgdd_air <- ave(expphenclim$gdd_air,list(expphenclim$site,expphenclim$plot,expphenclim$year), FUN=cumsum)
expphenclim$cumgdd_soil <- ave(expphenclim$gdd_soil,list(expphenclim$site,expphenclim$plot,expphenclim$year), FUN=cumsum)
cumsum(ifelse(is.na(x), 0, x)) + x*0
write.csv(expphenclim, "radmeeting/expclim.csv", row.names=FALSE)
expphenclim
##Look at the data to check for errors
boxplot(as.numeric(airtemp_min)~site, data=expphenclim)
boxplot(as.numeric(airtemp_max)~site, data=expphenclim)
boxplot(as.numeric(soiltemp1_min)~site, data=expphenclim)
boxplot(as.numeric(soiltemp2_min)~temptreat, data=expphenclim)
boxplot(as.numeric(soiltemp1_max)~temptreat, data=expphenclim)
boxplot(as.numeric(soiltemp2_max)~temptreat, data=expphenclim)
boxplot(as.numeric(soiltemp1_mean)~preciptreat, data=expphenclim)
boxplot(as.numeric(soilmois)~site, data=expphenclim)

###unused code:
##air temp files from chuine (not on plot level, so not useful?)
maxtempfiles<-c("meteoTE05_maxtemp2005.csv","meteoTE04_maxtemp2004.csv","meteoTE03_maxtemp2003.csv","meteoTE02_maxtemp2002.csv")
mintempfiles<-c("meteoTE05_mintemp2005.csv","meteoTE04_mintemp2004.csv","meteoTE03_mintemp2003.csv","meteoTE02_mintemp2002.csv")
temp <- NA
for (i in 1:length(maxtempfiles)){
  file <- file.path(path, paste(maxtempfiles[i]))
  maxtemp1 <- read.csv(file, skip=1,header=TRUE)
  colnames(maxtemp1)[1:13]<-c("date","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  maxtemp2<-subset(maxtemp1[1:31,],select=c("date","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  maxtemp.long<-reshape(maxtemp2,varying = list(colnames(maxtemp2)[2:13]), direction = "long", v.names = c("maxairtemp"), times = c(colnames(maxtemp2)[2:13]))
  colnames(maxtemp.long)[2]<-"month"
  file2 <- file.path(path, paste(mintempfiles[i]))
  mintemp1 <- read.csv(file2, skip=1,header=TRUE)
  colnames( mintemp1)[1:13]<-c("date","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  mintemp2<-subset(mintemp1[1:31,],select=c("date","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  mintemp.long<-reshape(mintemp2,varying = list(colnames(mintemp2)[2:13]), direction = "long", v.names = c("minairtemp"), times = c(colnames(mintemp2)[2:13]))
  colnames(mintemp.long)[2]<-"month"
  allairtemp <- merge(maxtemp.long,mintemp.long)