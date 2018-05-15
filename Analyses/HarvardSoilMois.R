#Looking at Harvard forest soil moisture data
#Goals are:
#1) See if relationship between temperature and soil moisture is different than 
#relationship among experiments. 
#Can also do this with controls in experimental dataset
#2) See if relationship between Harvard Forest phenology and soil moisture/temperature differs from experimental dataset
#Started by  Ailene, August 23 2017

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
#load libraries:
library(dplyr)
library(lme4)

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
harvagt.yr<-cbind(harvagmax.yr,harvagmin.yr[,2])
colnames(harvagt.yr)<-c("year","tmax","tmin")
#Get annual values of soil moisture (no precip data for HF i think....)
harvsoil$year<-substr(strptime(as.Date(harvsoil$date,format = "%M/%d/%y"),format = "%Y"),1,4)
harvsoil$year<-as.integer(harvsoil$year)

harvsoil.yr<-aggregate(harvsoil$vsm, by=list(harvsoil$year), FUN=mean,na.rm=TRUE)
colnames(harvsoil.yr)<-c("year","vsm")
harvsoil.yr$year<-as.integer(harvsoil.yr$year)
harvagt.yr$year<-as.integer(harvagt.yr$year)
#Merge daily soil moisture and with daily airtemp valuesLook at effect of air on soil moisture. Add annual temp values to soil data
harvsoil2<-left_join(harvsoil,harvair,by=c("year","doy"))
dim(harvsoil2)
#Merge daily soil moisture and with annual airtemp valuesLook at effect of air on soil moisture. Add annual temp values to soil data
harvsoil3<-left_join(harvsoil,harvagt.yr,by=c("year"), copy=TRUE)

#Annual climate data for soil moisture and temperature, to be used in phenology model
harvclim<-left_join(harvagt.yr,harvsoil.yr,by=c("year"), copy=TRUE)

#Fit model. Want model to be as analagous as possible to experimental model, which was:
#sm_mod<-lmer(soilmois1~target*preciptreat_amt + (1|site/year/doy), REML=FALSE, data=expclim2a)
harvsoil2$site<-as.factor(harvsoil2$site)
harvsoil2$doy<-as.factor(harvsoil2$doy)#

harvsm_mod<-lmer(vsm~tmax + (1|site)+(1|year/doy), REML=FALSE, data=harvsoil3)
summary(harvsm_mod)#temp coef: tmax: -0.02893
#experimental model: target -3.517e-03

#To do this, get annual average temperature and use this in place of "target" then get total annual precipitation, and use this in place of "preciptreat_amt
#