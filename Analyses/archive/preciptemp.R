#Additional analyses for Experimental Climate Paper
#Analysis of precipitation treatment effects on temperature
#By Ailene Ettinger
#Started 7 november 2017
setwd("~/git/radcliffe/Analyses")
rm(list=ls()) 
options(stringsAsFactors=FALSE)

library(lme4)
library(car)
library(raster)
library(RColorBrewer)
library(dplyr)
library(tidyr)

expclim<-read.csv("expclim.csv", header=T)
treats<-read.csv("treats_detail.csv", header=T)
#want to compare temperature in plots with and without precipitation treatments. 
expclim2<-full_join(treats,expclim, by=c("site", "block", "plot","temptreat","preciptreat"), match="first")
expclim2[,unique(expclim2$preciptreat,by=expclim2$site)]
expclim2$alltreats<-paste(expclim2$site,expclim2$temptreat,expclim2$preciptreat)
preciptreats <- expclim2 %>% # start with the data frame
  distinct(alltreats, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(site, temptreat, preciptreat)
psites<-unique(preciptreats$site[!is.na(preciptreats$preciptreat)])
#only include studies that manipulate precipitation
expclimp<-expclim2[which(expclim2$site=="exp01"|expclim2$site=="exp02"|expclim2$site=="exp05"|expclim2$site=="exp09"|expclim2$site=="exp12"),]
#get one column for above-ground temperature
expclimp$agtemp_min<-expclimp$airtemp_min
expclimp[which(is.na(expclimp$agtemp_min) & !is.na(expclimp$cantemp_min)),]$agtemp_min<-expclimp[which(is.na(expclimp$airtemp_min) & !is.na(expclimp$cantemp_min)),]$cantemp_min
expclimp[which(is.na(expclimp$agtemp_min) & !is.na(expclimp$surftemp_min)),]$agtemp_min<-expclimp[which(is.na(expclimp$agtemp_min) & !is.na(expclimp$surftemp_min)),]$surftemp_min
expclimp$agtemp_max<-expclimp$airtemp_max
expclimp[which(is.na(expclimp$agtemp_max) & !is.na(expclimp$cantemp_max)),]$agtemp_max<-expclimp[which(is.na(expclimp$airtemp_max) & !is.na(expclimp$cantemp_max)),]$cantemp_max
expclimp[which(is.na(expclimp$agtemp_max) & !is.na(expclimp$surftemp_max)),]$agtemp_max<-expclimp[which(is.na(expclimp$agtemp_max) & !is.na(expclimp$surftemp_max)),]$surftemp_max
expclimp$target[which(is.na(expclimp$target))]<-0

expclimp$preciptreat_amt[which(is.na(expclimp$preciptreat_amt))]<-0
expclimp$preciptreat_amt<-as.numeric(expclimp$preciptreat_amt)
expclimp$preciptreat_prop<-expclimp$preciptreat_amt/100
table(expclimp$site, expclimp$preciptreat_amt)

#check which sites have agtemp 
expclimp<-subset(expclimp,select=c(site,year,doy,preciptreat_amt,preciptreat_prop,target,agtemp_min,agtemp_max))
expclimp  <- expclimp [apply(expclimp , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
table(expclimp$site, expclimp$preciptreat_amt)

tempmod<-lmer(agtemp_max~preciptreat_amt + (1|site/year/doy), data=expclimp, REML=FALSE)
summary(tempmod)
tempmod1<-lmer(agtemp_max~target + (1|site/year/doy), data=expclimp, REML=FALSE)
summary(tempmod)
#tempmod2<-lmer(agtemp_max~preciptreat_amt + (preciptreat_amt|site/year/doy), data=expclimp, REML=FALSE)
#summary(tempmod2)#failed to converge
tempmod3<-lmer(agtemp_max~preciptreat_amt + target+ (1|site/year/doy), data=expclimp, REML=FALSE)
summary(tempmod3)
tempmod4<-lmer(agtemp_max~preciptreat_amt * target+ (1|site/year/doy), data=expclimp, REML=FALSE)
summary(tempmod4)

AIC(tempmod,tempmod1,tempmod3,tempmod4)# tempmod4 wins
tempmod4_min<-lmer(agtemp_min~preciptreat_amt * target+ (1|site/year/doy), data=expclimp, REML=FALSE)
summary(tempmod4_min)
