#Additional analyses for Experimental Climate Paper
#Analysis of Soil moisture in warmed plotrs
#By Ailene Ettinger
#Started 17 January 2017
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

#only include studies that manipulate precipitation
preciptreats<-table(treats$site, treats$preciptreat)
unique(preciptreats)
#select only rows that do not manipulated precipitation
expclimt<-expclim2[which(expclim2$preciptreat==0|is.na(expclim2$preciptreat)),]
#get one column for above-ground temperature
expclimt$agtemp_min<-expclimt$airtemp_min
expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$cantemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$airtemp_min) & !is.na(expclimt$cantemp_min)),]$cantemp_min
expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$surftemp_min
expclimt$agtemp_max<-expclimt$airtemp_max
expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$cantemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$airtemp_max) & !is.na(expclimt$cantemp_max)),]$cantemp_max
expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$surftemp_max
expclimt$styear<-NA#
#make a column for styear (study year, as opposed to calendar year)
sites<-unique(expclimt$site)
for (i in 1:length(sites)){
  sitedat<-expclimt[expclimt$site==sites[i],]
  styears<-unique(sitedat$year)
  #print(styears)
  for (j in 1:length(styears)){
    expclimt$styear[expclimt$site==sites[i] & expclimt$year==styears[j]]<-j
  }
}

expclimt$temptreat <- relevel(as.factor(expclimt$temptreat), ref = "ambient")
moismod2<-lmer(soilmois1~temptreat + (1|site/year), data=expclimt, REML=FALSE)
#the below model takes a really long time...
moismod<-lmer(soilmois1~temptreat + (temptreat|site/styear), data=expclimt, REML=FALSE)
AIC(moismod,moismod2)#moismod2 wins
summary(moismod2)
expclimt$warm<-"warmed"#for actively warmed sites
expclimt[which(expclimt$temptreat=="0"),]$warm<-"struc_cont"#for structural controls
expclimt[which(expclimt$temptreat=="ambient"),]$warm<-"amb_cont"#for ambient controls, which will be the reference
moismod3<-lmer(soilmois1~warm + (1|site/year), data=expclimt, REML=FALSE)
moismod<-lmer(soilmois1~airtemp_max + (1|site/year), data=expclimt, REML=FALSE)
summary(moismod)
