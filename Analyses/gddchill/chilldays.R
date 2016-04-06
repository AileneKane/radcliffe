#Code to estimate chilling days 
options(stringsAsFactors=FALSE)
library(RColorBrewer)
library(lme4)
library(car)
##Read in climate and phenology data
setwd("~/GitHub/radcliffe")
expclim<-read.csv("Analyses/expclim.csv", header=T)

##Now calculate chilling days. First, use a base of 5 (<5 degrees C=chilling day)
#For all sites/years/plots that we have data from previous year to sept 1, calculate chilling days
#First select out rows for which we have data from the previous year to sept 1
expclim<-expclim[order(expclim$site,expclim$plot,expclim$year, expclim$doy),]
tchill<-5
expclim$sept30doy<-as.numeric(strftime(strptime(paste(expclim$year,9,1,sep="-"),format = "%Y-%m-%d"),format = "%j"))
expclim$dec31doy<-(as.numeric(strftime(strptime(paste(expclim$year,12,31,sep="-"),format = "%Y-%m-%d"),format = "%j")))
late<-subset(expclim,expclim$doy>=sept30doy)
late$chyr<-late$year+1
late$chdoy<-late$doy-late$sept30doy+1
early<-subset(expclim,expclim$doy<as.numeric(strftime(strptime(paste(expclim$year,9,1,sep="-"),format = "%Y-%m-%d"),format = "%j")))
early$chyr= early$year
early$chdoy<-early$doy+(early$dec31doy-early$sept30doy)
expclim_new<-rbind(late, early)
expclim_new$chday_soil<-NA
##The below code is really slow!First get chill days based on mean soil temp
for (i in 1:dim(expclim_new)[1]){
  if(!is.na(expclim_new$soiltemp1_mean[i])){
    if(expclim_new$soiltemp1_mean[i]<tchill){expclim_new$chday_soil[i]<-1}
    if(expclim_new$soiltemp1_mean[i]>=tchill){expclim_new$chday_soil[i]<-0}
  }
}
##The below code is really slow!Then get chill days based on mean airtmep
expclim_new$airtemp_mean<-(expclim_new$airtemp_min+expclim_new$airtemp_max)/2
for (i in 1:dim(expclim_new)[1]){
  if(!is.na(expclim_new$airtemp_mean[i])){
    if(expclim_new$airtemp_mean[i]<tchill){expclim_new$chday_air[i]<-1}
    if(expclim_new$airtemp_mean[i]>=tchill){expclim_new$chday_air[i]<-0}
  }
}
#now sum everything up!
expclim_new<-expclim_new[order(expclim_new$site,expclim_new$plot,expclim_new$chyr, expclim_new$chdoy),]
cumsumnona <- function(x){cumsum(ifelse(is.na(x), 0, x)) + x*0}
countcumna <- function(x){ cumsum (is.na(x))}

expclim_new$cumchill_soil<-ave(expclim_new$chday_soil,list(expclim_new$site,
      expclim_new$plot,expclim_new$chyr), FUN=cumsumnona)
expclim_new$numnas_soil<-ave(expclim_new$chday_soil,list(expclim_new$site,
     expclim_new$plot,expclim_new$chyr), FUN=countcumna)

expclim_new$cumchill_air<-ave(expclim_new$chday_air,list(expclim_new$site,
       expclim_new$plot,expclim_new$chyr), FUN=cumsumnona)
expclim_new$numnas_air<-ave(expclim_new$chday_air,list(expclim_new$site,
        expclim_new$plot,expclim_new$chyr), FUN=countcumna)

write.csv(expclim_new,"expchill.csv", row.names=FALSE)
