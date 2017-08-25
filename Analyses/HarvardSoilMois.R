#Looking at Harvard forest soil moisture data
#Goals are:
#1) See if relationship between temperature and soil moisture is different than relationship among experiments. Can also do this with controls in experimental dataset
#2) See if relationship between HArvard Forest phenology and soil moisture/temperature differs from experimental dataset
#Started by  Ailene, August 23 2017

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
#load libraries:
library(dplyr)

##Read in harvard soil and phenology data
setwd("~/git/radcliffe")
harvsoil<-read.csv("Data/Other/hf006-01-soil-respiration.csv", header=TRUE)
harvair<-read.csv("Data/Other/hf004-02-filled.csv", header=TRUE)
pheno<-read.csv("Analyses/obspheno.csv", header=TRUE)
harvpheno<-pheno[pheno$site=="harvard",]
#dim(harvpheno)
#get daily max temp
harvagmax<-aggregate(harvair$ta.27m.filled, by=list(harvair$year,harvair$month,harvair$doy), FUN=max,na.rm=TRUE)
harvagmin<-aggregate(harvair$ta.27m.filled, by=list(harvair$year,harvair$month,harvair$doy), FUN=min,na.rm=TRUE)
colnames(harvagmax)<-c("year","month","doy","airTmax")
colnames(harvagmin)<-c("year","month","doy","airTmin")
#Now get annual values of air temperature (no precip data for HF i think....)
harvagmax.yr<-aggregate(harvagmax$airTmax, by=list(harvagmax$year), FUN=mean,na.rm=TRUE)
harvagmin.yr<-aggregate(harvagmin$airTmin, by=list(harvagmin$year), FUN=mean,na.rm=TRUE)

#add year column to harvsoil so that i can merge them
harvsoil$year<-strptime(as.Date(harvsoil$date,format = "%M/%d/%y"),format = "%Y")
as.Date("01/01/2009", format = "%m/%d/%Y"); lubridate::year(x). – Roman Luštrik Apr 12 '16 at 8:57 
#Look at effect of air on soil moisture. Add annual temp values to soil data
harvsoil2<-left_join(harvsoil,harvagmax.yr,by=c("year"), copy=TRUE)


dim(expgdd2)#59675    53

harvsoil2<-join(harvsoil,harvagmax.yr,by=year)
#Fit model. Want model to be as analagous as possible to experimental model, which was:
#sm_mod<-lmer(soilmois1~target + (1|site/year/doy), REML=FALSE, data=expclim2a)
#To do this, get annual average temperature and use this in place of "target" then get total annual precipitation, and use this in place of "preciptreat_amt
#