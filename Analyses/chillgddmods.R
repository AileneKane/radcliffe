#Code to estimate chilling days 
options(stringsAsFactors=FALSE)
library(RColorBrewer)
library(lme4)
library(car)
##Read in climate and phenology data
setwd("~/GitHub/radcliffe/Analyses")
expclim<-read.csv("expclim.csv", header=T)

##Now calculate chilling days. First, use a base of 5 (<5 degrees C=chilling day)
#For all sites/years/plots that we have data from previous year to sept 1, calculate chilling days
#First select out rows for which we have data from the previous year to sept 1
expclim<-expclim[order(expclim$site,expclim$plot,expclim$year, expclim$doy),]
tchill<-5
late<-subset(expclim,expclim$doy>=as.numeric(strftime(strptime(paste(expclim$year,9,1,sep="-"),format = "%Y-%m-%d"),format = "%j")))
late$chyr<-late$year+1
late$chdoy<-late$doy-as.numeric(strftime(strptime(paste(late$year,9,1,sep="-"),format = "%Y-%m-%d"),format = "%j"))+1
early<-subset(expclim,expclim$doy<as.numeric(strftime(strptime(paste(expclim$year,9,1,sep="-"),format = "%Y-%m-%d"),format = "%j")))
early$chyr= early$year
early$chdoy<-early$doy-as.numeric(strftime(strptime(paste(early$year,9,1,sep="-"),format = "%Y-%m-%d"),format = "%j"))-1
expclim_new<-rbind(late, early)
expclim_new$chday<-NA
expclim_new<-expclim_new[order(expclim_new$site,expclim_new$plot,expclim_new$chyr, expclim_new$chdoy),]

##The below code is really slow!
for (i in 1:dim(expclim_new)[1]){
  if(!is.na(expclim_new$soiltemp1_mean[i])){
    if(expclim_new$soiltemp1_mean[i]<tchill){expclim_new$chday[i]<-1}
    if(expclim_new$soiltemp1_mean[i]>=tchill){expclim_new$chday[i]<-0}
  }
}
  
cumsumnona <- function(x){cumsum(ifelse(is.na(x), 0, x)) + x*0}
countcumna <- function(x){cumsum((is.na(x))}

expclim_new$cumchill<-ave(expclim_new$chday,list(expclim_new$site,
      expclim_new$plot,expclim_new$chyr), FUN=cumsumnona)
expclim_new$numnas<-ave(expclim_new$chday,list(expclim_new$site,
     expclim_new$plot,expclim_new$chyr), FUN=countcumna)

write.csv(expclim_new,"expgddchill.csv", row.names=FALSE)
