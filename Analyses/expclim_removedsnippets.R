###Rcode snippet
###Ailene
###used to estimate hourly differences between treatments in BACe experiments. removed from expclim because i wanted to switch to daily values to remove extreme values
##updated April 5, 2016 with more years of soil data, and adding air data
clean.clim$bace <- function(path="./Data/Experiments/bace") {
  bacesoiltempfiles<-c("2009SoilTempDaily.csv","2010SoilTempDaily.csv","2011SoilTempDaily.csv","2012SoilTempDaily.csv")
  bacesoilmoisfiles<-c("2009_0_30cmTDR12_29_09CEG.csv","bace_soilmoisture2010.csv","bace_soilmoisture2011.csv",
                       "2012_0_30cmTDR_1_23_13_CEG.csv","2013_30cmTDR3_7_15SWW.csv","2014_30cmTDR3_7_15SWW.csv")
  bacecantempfiles_hr<-c("2009PlotTempHourly12_01_11CEG.csv","2010PlotTempHourly05_27_11_CEG.csv",
                         "2011PlotTempHourly1_6_12_CEG.csv","2012PlotTempHourly2_1_13_CEG.csv")
  bacesoiltempfiles_hr<-c("2009SoilTempHourly.csv","2010SoilTempHourly.csv","2011SoilTempHourly.csv","2012SoilTempHourly.csv")
  allsoiltemp<-c()
  for (i in 1:length(bacesoiltempfiles)){
    file <- file.path(path, paste(bacesoiltempfiles[i]))
    soiltemp <- read.csv(file, header=TRUE,na.strings = ".")
    colnames(soiltemp) [3:5]<-c("doy","plot","temptreat")
    colnames(soiltemp) [9:12]<-c("soiltemp1_min","soiltemp2_min","soiltemp1_max","soiltemp2_max")
    soiltemp<-soiltemp[,1:12]
    if(i==4){soiltemp<-soiltemp[1:min(which(is.na(soiltemp$year)))-1,1:12]}
    soiltemp$preciptreat<-NA
    soiltemp[soiltemp$precip.treatment==1,]$preciptreat<-0#ambient precip
    soiltemp[soiltemp$precip.treatment==0,]$preciptreat<--1#50% precip
    soiltemp[soiltemp$precip.treatment==2,]$preciptreat<-1#150% ambient precip
    allsoiltemp<-rbind(allsoiltemp,soiltemp)
  }
  allsoilcantemp_hr<-c()
  for (j in 1:length(bacecantempfiles)){
    file <- file.path(path, paste(bacecantempfiles_hr[j]))
    file2<- file.path(path, paste(bacesoiltempfiles_hr[j]))
    cantemp <- read.csv(file, header=TRUE,na.strings = ".")
    cantemp<-cantemp[!is.na(cantemp$plot),1:which(colnames(cantemp)=="hour.23.max")]
    cantemp$preciptreat<-NA
    cantemp[cantemp$precip.treatment==1,]$preciptreat<-0#ambient precip
    cantemp[cantemp$precip.treatment==0,]$preciptreat<--1#50% precip
    cantemp[cantemp$precip.treatment==2,]$preciptreat<-1#150% ambient precip
    cantemp$block<-NA
    cantemp[cantemp$plot<5,]$block<-1
    cantemp[cantemp$plot<9 & cantemp$plot>4,]$block<-2
    cantemp[cantemp$plot<13 & cantemp$plot>8,]$block<-3
    cantemp[cantemp$plot<17 & cantemp$plot>12,]$block<-4
    cantemp[cantemp$plot<21 & cantemp$plot>16,]$block<-5
    cantemp[cantemp$plot<25 & cantemp$plot>20,]$block<-6
    cantemp[cantemp$plot<29 & cantemp$plot>24,]$block<-7
    cantemp[cantemp$plot<33 & cantemp$plot>28,]$block<-8
    cantemp[cantemp$plot<37 & cantemp$plot>32,]$block<-9
    #unique(cantemp$plot)
    #measured canopy temps (for control (0) and high warming treatment (3)):
    cantemp_min<-subset(cantemp,select=c("year","day","block","plot","warming.treatment","preciptreat",colnames(cantemp)[which(substr(colnames(cantemp),nchar(colnames(cantemp))-2,nchar(colnames(cantemp)))=="min")]))
    cantemp_max<-subset(cantemp,select=c("year","day","block","plot","warming.treatment","preciptreat",colnames(cantemp)[which(substr(colnames(cantemp),nchar(colnames(cantemp))-2,nchar(colnames(cantemp)))=="max")]))
    cantemp_min.l<-reshape(cantemp_min,varying=list(colnames(cantemp_min)[7:30]),direction="long",v.names=c("cantempmin"),times=c(substr(colnames(cantemp_min)[7:30],6,nchar(colnames(cantemp_min)[7:30])-4)))
    cantemp_max.l<-reshape(cantemp_max,varying=list(colnames(cantemp_max)[7:30]),direction="long",v.names=c("cantempmax"),times=c(substr(colnames(cantemp_max)[7:30],6,nchar(colnames(cantemp_max)[7:30])-4)))
    cantemp_min.l<-cantemp_min.l[order(cantemp_min.l$warming.treatment,cantemp_min.l$preciptreat,cantemp_min.l$year,cantemp_min.l$day,cantemp_min.l$time),]
    cantemp_min.l$canminreftemp<-c(cantemp_min.l$cantempmin[1:(dim(cantemp)[1]*12)],cantemp_min.l$cantempmin[1:(dim(cantemp)[1]*12)])
    cantemp_min.l$canmindelta<- cantemp_min.l$cantempmin-cantemp_min.l$canminreftemp
    cantemp_max.l<-cantemp_max.l[order(cantemp_max.l$warming.treatment,cantemp_max.l$preciptreat,cantemp_max.l$year,cantemp_max.l$day,cantemp_max.l$time),]
    cantemp_max.l$canmaxreftemp<-c(cantemp_max.l$cantempmax[1:(dim(cantemp)[1]*12)],cantemp_max.l$cantempmax[1:(dim(cantemp)[1]*12)])
    cantemp_max.l$canmaxdelta<-cantemp_max.l$cantempmax-cantemp_max.l$canmaxreftemp
    #cantemp_min.l[which(cantemp_min.l$canmindelta<0),]$canmindelta<-NA
    #cantemp_max.l[which(cantemp_max.l$canmaxdelta<0),]$canmaxdelta<-NA
    cantemp_minmax<-cbind(cantemp_min.l,cantemp_max.l$cantempmax,cantemp_max.l$canmaxreftemp,cantemp_max.l$canmaxdelta)
    colnames(cantemp_minmax)[12:14]<-c("cantempmax","canmaxreftemp","canmaxdelta")
    rownames(cantemp_minmax)<-NULL
    cantemp_minmax<-cantemp_minmax[,-which(colnames(cantemp_minmax)=="id")]
    
    #estimated canopy temps, using difference between control and high warming treatment and soil temperature differences
    soiltemp<-read.csv(file2, header=TRUE,na.strings = ".")
    soiltemp<-soiltemp[!is.na(soiltemp$plot),1:which(colnames(soiltemp)=="Hour.23..10.cm.max")]
    soiltemp$preciptreat<-NA
    soiltemp[soiltemp$precip.treatment==1,]$preciptreat<-0#ambient precip
    soiltemp[soiltemp$precip.treatment==0,]$preciptreat<--1#50% precip
    soiltemp[soiltemp$precip.treatment==2,]$preciptreat<-1#150% ambient precip
    soiltemp$block<-NA
    soiltemp[soiltemp$plot<5,]$block<-1
    soiltemp[soiltemp$plot<9 & soiltemp$plot>4,]$block<-2
    soiltemp[soiltemp$plot<13 & soiltemp$plot>8,]$block<-3
    soiltemp[soiltemp$plot<17 & soiltemp$plot>12,]$block<-4
    soiltemp[soiltemp$plot<21 & soiltemp$plot>16,]$block<-5
    soiltemp[soiltemp$plot<25 & soiltemp$plot>20,]$block<-6
    soiltemp[soiltemp$plot<29 & soiltemp$plot>24,]$block<-7
    soiltemp[soiltemp$plot<33 & soiltemp$plot>28,]$block<-8
    soiltemp[soiltemp$plot<37 & soiltemp$plot>32,]$block<-9
    #select out shallower soiltemp depth only:
    soiltemp_min<-subset(soiltemp,select=c("year","day","block","plot","warming.treatment","preciptreat",colnames(soiltemp)[which(substr(colnames(soiltemp),nchar(colnames(soiltemp))-7,nchar(colnames(soiltemp)))=="2.cm.min")]))
    soiltemp_max<-subset(soiltemp,select=c("year","day","block","plot","warming.treatment","preciptreat",colnames(soiltemp)[which(substr(colnames(soiltemp),nchar(colnames(soiltemp))-7,nchar(colnames(soiltemp)))=="2.cm.max")]))
    soiltemp_min.l<-reshape(soiltemp_min,varying=list(colnames(soiltemp_min)[7:30]),direction="long",v.names=c("soiltempmin"),times=c(substr(colnames(soiltemp_min)[7:30],6,nchar(colnames(soiltemp_min)[7:30])-10)))
    soiltemp_max.l<-reshape(soiltemp_max,varying=list(colnames(soiltemp_max)[7:30]),direction="long",v.names=c("soiltempmax"),times=c(substr(colnames(soiltemp_max)[7:30],6,nchar(colnames(soiltemp_max)[7:30])-10)))
    #calculate difference between treatment and control at each hour
    soiltemp_min.l$soilminreftemp<-NA
    soiltemp_min.l$soilmindelta<-NA
    soiltemp_max.l$soilmaxreftemp<-NA
    soiltemp_max.l$soilmaxdelta<-NA
    soiltemp_min.l<-soiltemp_min.l[order(soiltemp_min.l$warming.treatment,soiltemp_min.l$preciptreat,soiltemp_min.l$year,soiltemp_min.l$day,soiltemp_min.l$time),]
    soiltemp_min.l$soilminreftemp<-c(soiltemp_min.l$soiltempmin[1:(dim(soiltemp)[1]*6)],soiltemp_min.l$soiltempmin[1:(dim(soiltemp)[1]*6)],soiltemp_min.l$soiltempmin[1:(dim(soiltemp)[1]*6)],soiltemp_min.l$soiltempmin[1:(dim(soiltemp)[1]*6)])
    soiltemp_min.l$soilmindelta<- soiltemp_min.l$soiltempmin-soiltemp_min.l$soilminreftemp
    soiltemp_max.l<-soiltemp_max.l[order(soiltemp_max.l$warming.treatment,soiltemp_max.l$preciptreat,soiltemp_max.l$year,soiltemp_max.l$day,soiltemp_max.l$time),]
    soiltemp_max.l$soilmaxreftemp<-c(soiltemp_max.l$soiltempmax[1:(dim(soiltemp)[1]*6)],soiltemp_max.l$soiltempmax[1:(dim(soiltemp)[1]*6)],soiltemp_max.l$soiltempmax[1:(dim(soiltemp)[1]*6)],soiltemp_max.l$soiltempmax[1:(dim(soiltemp)[1]*6)])
    soiltemp_max.l$soilmaxdelta<- soiltemp_max.l$soiltempmax-soiltemp_max.l$soilmaxreftemp
    #soiltemp_min.l[which(soiltemp_min.l$soilmindelta<0),]$soilmindelta<-NA
    #soiltemp_max.l[which(soiltemp_max.l$soilmaxdelta<0),]$soilmaxdelta<-NA
    soiltemp_minmax<-cbind(soiltemp_min.l,soiltemp_max.l$soiltempmax,soiltemp_max.l$soilmaxreftemp,soiltemp_max.l$soilmaxdelta)
    colnames(soiltemp_minmax)[12:14]<-c("soiltempmax","soilmaxreftemp","soilmaxdelta")
    soiltemp_minmax<-soiltemp_minmax[,-which(colnames(soiltemp_minmax)=="id")]
    rownames(soiltemp_minmax)<-NULL
    soilcantemp_hr<-merge(soiltemp_minmax,cantemp_minmax,by=c("year","day","block","plot","warming.treatment","preciptreat","time"),all=TRUE)
    #now get estimate for missing canopy temps, based on difference on deltas in soil temps
    quartz()
    boxplot(soilcantemp_hr$cantempmin~soilcantemp_hr$warming.treatment, main=paste("before",bacesoiltempfiles[j]))
    boxplot(soilcantemp_hr$cantempmax~soilcantemp_hr$warming.treatment, main=paste("before",bacesoiltempfiles[j]))
    boxplot(soilcantemp_hr$canmaxreftemp~soilcantemp_hr$warming.treatment, main=paste("before",bacesoiltempfiles[j]))
    boxplot(soilcantemp_hr$canminreftemp~soilcantemp_hr$warming.treatment, main=paste("before",bacesoiltempfiles[j]))
    
    soilcantemp_hr<-soilcantemp_hr[order(soilcantemp_hr$warming.treatment,soilcantemp_hr$year,soilcantemp_hr$day,soilcantemp_hr$time),]
    Cmin<-soilcantemp_hr[soilcantemp_hr$warming.treatment==0,]$canminreftemp
    Cmax<-soilcantemp_hr[soilcantemp_hr$warming.treatment==0,]$canmaxreftemp
    DImin<-soilcantemp_hr[soilcantemp_hr$warming.treatment==3,]$canmindelta
    DImax<-soilcantemp_hr[soilcantemp_hr$warming.treatment==3,]$canmaxdelta
    DHmin<-soilcantemp_hr[soilcantemp_hr$warming.treatment==3,]$soilmindelta
    DHmax<-soilcantemp_hr[soilcantemp_hr$warming.treatment==3,]$soilmaxdelta
    soilcantemp_hr[soilcantemp_hr$warming.treatment>0 & soilcantemp_hr$warming.treatment<3,]$cantempmin<- Cmin+(DImin*(soilcantemp_hr[soilcantemp_hr$warming.treatment>0 &soilcantemp_hr$warming.treatment<3,]$soilmindelta/(c(soilminrefdeltatemp,soilminrefdeltatemp))))
    soilcantemp_hr[soilcantemp_hr$warming.treatment>0 & soilcantemp_hr$warming.treatment<3,]$cantempmax<-Cmax+(DImax*(soilcantemp_hr[soilcantemp_hr$warming.treatment>0 &soilcantemp_hr$warming.treatment<3,]$soilmaxdelta/(c(soilmaxrefdeltatemp,soilmaxrefdeltatemp))))
    #remove weird outliers (those above the max temp measured in the high warming plot, and those below the lowest temp measured in the control plot) and replace with NAs
    soilcantemp_hr[which(soilcantemp_hr$cantempmin>max(soilcantemp_hr[soilcantemp_hr$warming.treatment==3,]$cantempmin, na.rm=T)),]$cantempmin<-NA
    soilcantemp_hr[which(soilcantemp_hr$cantempmax>max(soilcantemp_hr[soilcantemp_hr$warming.treatment==3,]$cantempmax, na.rm=T)),]$cantempmax<-NA
    soilcantemp_hr[which(soilcantemp_hr$cantempmin<min(soilcantemp_hr[soilcantemp_hr$warming.treatment==3,]$cantempmin, na.rm=T)),]$cantempmin<-NA
    soilcantemp_hr[which(soilcantemp_hr$cantempmax<min(soilcantemp_hr[soilcantemp_hr$warming.treatment==3,]$cantempmax, na.rm=T)),]$cantempmax<-NA
    
    quartz()
    boxplot(soilcantemp_hr$cantempmin~soilcantemp_hr$warming.treatment, main=paste("after",bacesoiltempfiles[j]))
    allsoilcantemp_hr<-rbind(allsoilcantemp_hr,soilcantemp_hr)
  }#j
  allsoilmois<-c()
  for (i in 1:length(bacesoilmoisfiles)){
    file2 <- file.path(path, paste(bacesoilmoisfiles[i]))
    soilmois<-read.csv(file2, header=T,na.strings = ".")
    colnames(soilmois)[3]<-"doy"
    soilmois<-soilmois[,1:7]
    allsoilmois<-rbind(allsoilmois,soilmois)
  }
  allclim<-merge(allsoiltemp,allsoilmois,by=c("year","doy","plot"),all=TRUE) 
  colnames(allclim)[17]<-"soilmois1"
  allclim[which(allclim$soilmois1==209),]$soilmois1<-NA#remove weird values for soil moisture, which should be between 0 and 1 (209, 1.87)
  allclim[which(allclim$soilmois1==1.87),]$soilmois1<-NA
  allclim$soilmois2<-NA
  allclim$airtemp_min<-NA
  allclim$airtemp_max<-NA
  allclim$site<-"bace"
  allclim$soiltemp1_mean<-(as.numeric(allclim$soiltemp1_min)+as.numeric(allclim$soiltemp1_max))/2
  baceclim<-subset(allclim, select=c("site","temptreat","preciptreat","plot","year","doy","airtemp_min","airtemp_max","soiltemp1_min","soiltemp2_min","soiltemp1_max","soiltemp2_max","soiltemp1_mean","soilmois1","soilmois2"))
  row.names(baceclim) <- NULL
  return(baceclim) 
}