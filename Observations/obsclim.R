### Started 18 March 2016 ##
### By Ailene Ettinger ###
setwd("~/GitHub/radcliffe")
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(reshape)
library(zoo)
##Daily temp data for observational studies, get GDD
clean.obsclim <- list()
path="./Observations/Temp"
tmaxfiles<-c("tmax_augspurger.csv","tmax_bock.csv","tmax_bolmgren.csv","tmax_concord.csv","tmax_fargo.csv","tmax_fitter.csv","tmax_gothic.csv","tmax_harvard.csv","tmax_hubbard.csv","tmax_konza.csv","tmax_marsham.csv","tmax_mikesell.csv","tmax_mohonk.csv","tmax_niwot.csv","tmax_rousi.csv","tmax_siernev1.csv","tmax_siernev2.csv","tmax_uwm.csv","tmax_washdc.csv","tmax_zacken.csv")
tminfiles<-c("tmin_augspurger.csv","tmin_bock.csv","tmin_bolmgren.csv","tmin_concord.csv","tmin_fargo.csv","tmin_fitter.csv","tmin_gothic.csv","tmin_harvard.csv","tmin_hubbard.csv","tmin_konza.csv","tmin_marsham.csv","tmin_mikesell.csv","tmin_mohonk.csv","tmin_niwot.csv","tmin_rousi.csv","tmin_siernev1.csv","tmin_siernev2.csv","tmin_uwm.csv","tmin_washdc.csv","tmin_zacken.csv")
sites<-c("augspurger","bock","bolmgren","concord","fargo","fitter","gothic","harvard","hubbard","konza","marsham","mikesell","mohonk","niwot","rousi","siernev1","siernev2","uwm","washdc","zacken")
tempallsites<-NA
for (i in 1:length(sites)){
    file1 <- file.path(path, paste(tmaxfiles[i]))
    file2 <- file.path(path, paste(tminfiles[i]))
    maxtemp1 <- read.csv(file1, header=TRUE)
    mintemp1 <- read.csv(file2, header=TRUE)
    colnames(maxtemp1)[1]<-"year"
    colnames(mintemp1)[1]<-"year"
    maxtemp1.long<-reshape(maxtemp1,varying = list(names(maxtemp1)[2:367]), direction = "long", v.names = c("tmax"), times = c(1:366))
    mintemp1.long<-reshape(mintemp1,varying = list(names(mintemp1)[2:367]), direction = "long", v.names = c("tmin"), times = c(1:366))
    alltemp<-merge(maxtemp1.long,mintemp1.long)
    colnames(alltemp)[2:3]<-c("doy","nyr")
    alltemp$site<-paste(sites[i])
    tempallsites<-rbind(tempallsites,alltemp)
}
  tempallsites<-tempallsites[-1,]
  tempallsites<-tempallsites[order(tempallsites$site,tempallsites$year, tempallsites$doy),]
  tbase<-10
  tempallsites$tmean<-(tempallsites$tmin+tempallsites$tmax)/2
  tempallsites$gdd_10<-tempallsites$tmean-tbase
  tempallsites$gdd_10[tempallsites$gdd_10 < 10] <- 0
  tempallsites$doy<-as.numeric(tempallsites$doy)
  tempallsites$cumgdd <- ave(tempallsites$gdd_10,list(tempallsites$site,tempallsites$year), FUN=cumsum)
  row.names(tempallsites) <- NULL
  write.csv(tempallsites,"radmeeting/obsclim.csv", row.names=FALSE)
