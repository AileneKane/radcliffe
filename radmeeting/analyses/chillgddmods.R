###Preliminary analyses of exp and obs data for radcliff
##Started March 18, 2016
options(stringsAsFactors=FALSE)
library(RColorBrewer)
library(lme4)
library(car)
##Read in climate and phenology data
setwd("~/GitHub/radcliffe/radmeeting")
expclim<-read.csv("expclim.csv", header=T)
exppheno<-read.csv("exppheno.csv", header=T)
obspheno<-read.csv("obspheno.csv", header=T)
head(expclim)

#Now, some preliminary analyses:
#first, calculate gdd to expclim file:
###Add GDD and cumulative gdd: soiltemp-tbase, cumulative GDD for that year (sum up to that date)
expclim<-expclim[order(expclim$site,expclim$plot,expclim$year, expclim$doy),]

tbase<-c(0,2,4,6,8,10)
for (i in 1:length(tbase)){
  expclim[,14+(i-1)+i]<-expclim$soiltemp1_mean-tbase[i]
  expclim[,15+(i-1)+i]<-((as.numeric(expclim$airtemp_min)+as.numeric(expclim$airtemp_max))/2)-tbase[i]
  expclim[,14+(i-1)+i][expclim[14+(i-1)+i] < tbase[i]] <- 0
  expclim[,15+(i-1)+i][expclim[15+(i-1)+i] < tbase[i]] <- 0
}
for (i in 1:length(tbase)){
  colnames(expclim)[14+(i-1)+i]<-paste("gdd_soil",tbase[i],sep=".")
  colnames(expclim)[15+(i-1)+i]<-paste("gdd_air",tbase[i],sep=".")
}
#check
#aggregate(expclim[, 15:24], list(expclim$site), mean, na.rm=T)
#now add columns for cumulative
cumsumnona <- function(x){cumsum(ifelse(is.na(x), 0, x)) + x*0}
for (i in 1:length(tbase)){
  expclim[,24+(i-1)+i]<-ave(expclim[,14+(i-1)+i],list(expclim$site,expclim$plot,expclim$year), FUN=cumsumnona)
  expclim[,25+(i-1)+i]<-ave(expclim[,15+(i-1)+i],list(expclim$site,expclim$plot,expclim$year), FUN=cumsumnona)
}
for (i in 1:length(tbase)){
  colnames(expclim)[24+(i-1)+i]<-paste("cumgdd_soil",tbase[i],sep=".")
  colnames(expclim)[25+(i-1)+i]<-paste("cumgdd_air",tbase[i],sep=".")
}
expclim$alltreat<-paste(expclim$temptreat,expclim$preciptreat,sep=".")

##Now calculate chilling days. First calculate all days that 
tchill<-5
expclim[which(expclim$soiltemp1_mean<tchill),]$chday<-1
expclim[which(expclim$soiltemp1_mean>=tchill),]$chday<-0
expclim$chillyear<-expclim$year
expclim[which(expclim$year>min(expclim$year)),]
expclim[expclim$doy >= as.numeric(strftime(strptime(paste(expclim$year,9,1,sep="-"), 
  format = "%Y-%m-%d"),format = "%j")),]$chillyear<-expclim[expclim$doy >= 
  as.numeric(strftime(strptime(paste(expclim$year,9,1,sep="-"), 
  format = "%Y-%m-%d"),format = "%j")),]$year+1
#if 
#For all sites/years/plots that we have data from previous year to sept 1, calculate chilling days
#First select out rows for which we have data from the previous year to sept 1
expclim[which()]