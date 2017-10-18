##Code to estimate chilling days
##by Ailene
##April 6, 2016
options(stringsAsFactors=FALSE)
#library(RColorBrewer)
#library(lme4)
#library(car)
##Read in climate and phenology data
setwd("~/git/radcliffe")
expclim<-read.csv("Analyses/expclim.csv", header=T)
##Calculate chilling days. We want to use a base of 5 (<5 degrees C=chilling day)
#For all sites/years/blocks/plots that we have data from previous year (starting sept 1), calculate chilling days from sept 1 through December 31
#First, create new column for chilling year and chilling doy  (starting sept 1 from the previous year)
expclim<-expclim[order(expclim$site,expclim$plot,expclim$year, expclim$doy),]
tchill<-5
expclim$sept01doy<-as.numeric(strftime(strptime(paste(expclim$year,9,1,sep="-"),format = "%Y-%m-%d"),format = "%j"))
expclim$dec31doy<-(as.numeric(strftime(strptime(paste(expclim$year,12,31,sep="-"),format = "%Y-%m-%d"),format = "%j")))
late<-subset(expclim,expclim$doy>=expclim$sept01doy)
late$chyr<-late$year+1
late$chdoy<-late$doy-late$sept01doy+1
early<-subset(expclim,expclim$doy<expclim$sept01doy)
early$chyr<-early$year
early$chdoy<-early$doy+(early$dec31doy-early$sept01doy)
expclim_new<-rbind(late, early)
expclim<-expclim_new
##First get chill days based on mean soil temp
expclim$chday_soil<-NA
expclim[!is.na(expclim$soiltemp1_mean) & expclim$soiltemp1_mean <tchill & expclim$doy>expclim$sept01doy,]$chday_soil <- 1
expclim[!is.na(expclim$soiltemp1_mean) & expclim$soiltemp1_mean >=tchill,]$chday_soil <- 0
expclim[!is.na(expclim$soiltemp1_mean) & expclim$doy<expclim$sept01doy,]$chday_soil <- 0

##Then get chill days based on mean airtmep
expclim$airtemp_mean<-(expclim$airtemp_min+expclim$airtemp_max)/2
expclim$chday_air<-NA
expclim[!is.na(expclim$airtemp_mean) & expclim$airtemp_mean <tchill & expclim$doy>=expclim$sept01doy,]$chday_air <- 1
expclim[!is.na(expclim$airtemp_mean) & expclim$airtemp_mean >=tchill,]$chday_air <- 0
expclim[!is.na(expclim$airtemp_mean) & expclim$doy<expclim$sept01doy,]$chday_air <- 0

##or, if mean air temp is not available, but canopy temperature is, use that
expclim$cantemp_mean<-(expclim$cantemp_min+expclim$cantemp_max)/2
expclim[is.na(expclim$airtemp_mean) & !is.na(expclim$cantemp_mean) & expclim$cantemp_mean <tchill & expclim$doy>=expclim$sept01doy,]$chday_air <- 1
expclim[is.na(expclim$airtemp_mean) & !is.na(expclim$cantemp_mean) & expclim$cantemp_mean >=tchill,]$chday_air <- 0
expclim[is.na(expclim$airtemp_mean) & !is.na(expclim$cantemp_mean) & expclim$doy<expclim$sept01doy,]$chday_air <- 0
#
#now sum everything up!
expclim2<-expclim[order(expclim$site,expclim$block,expclim$plot,expclim$chyr, expclim$chdoy),]
noblocksites<-unique(expclim2[which(is.na(expclim2$block)),]$site)
expclim2[which(is.na(expclim2$block)),]$block<-1

cumsumnona <- function(x){cumsum(ifelse(is.na(x), 0, x)) + x*0}
countcumna <- function(x){cumsum(is.na(x))}

expclim2$cumchill_soil<-NA
expclim2$cumchill_soil<-ave(expclim2$chday_soil,list(expclim2$site,expclim2$block,expclim2$plot,expclim2$chyr), FUN=cumsumnona)
expclim2$numnas_soil<-ave(expclim2$chday_soil,list(expclim2$site,expclim2$block,expclim2$plot,expclim2$chyr), FUN=countcumna)

expclim2$cumchill_air<-NA
expclim2$cumchill_air<-ave(expclim2$chday_air,list(expclim2$site,expclim2$block,expclim2$plot,expclim2$chyr), FUN=cumsumnona)
expclim2$numnas_air<-ave(expclim2$chday_air,list(expclim2$site,expclim2$block,expclim2$plot,expclim2$chyr), FUN=countcumna)

expclim2$chdoy_min<-ave(expclim2$chdoy,list(expclim2$site,expclim2$chyr), FUN=min)
###If no climate measurements taken prior Sept 15, then exclude chilldays (=NA) because not enough data to be reliable
expclim2[expclim2$chdoy_min>15,]$cumchill_air<-NA
expclim2[expclim2$chdoy_min>15,]$cumchill_soil<-NA
###############
##now add gdd##
###############
thresh <- 5 # set the base temp as 5
# Okay, need to calculate GDD for air mean and soil mean temps, starting january 1
# for each block x plot x site x year
expclim2$gdd_soil<-NA
expclim2[!is.na(expclim2$soiltemp1_mean) & expclim2$soiltemp1_mean >thresh,]$gdd_soil <- expclim2[!is.na(expclim2$soiltemp1_mean) & expclim2$soiltemp1_mean >thresh,]$soiltemp1_mean-thresh
expclim2[!is.na(expclim2$soiltemp1_mean) & expclim2$soiltemp1_mean <=thresh,]$gdd_soil <- 0

expclim2$gdd_air<-NA
expclim2[!is.na(expclim2$airtemp_mean) & expclim2$airtemp_mean >thresh,]$gdd_air <- expclim2[!is.na(expclim2$airtemp_mean) & expclim2$airtemp_mean >thresh,]$airtemp_mean-thresh
expclim2[!is.na(expclim2$airtemp_mean) & expclim2$airtemp_mean <=thresh,]$gdd_air <- 0

##or, if mean air temp is not available, but canopy temperature is, use that to calculate gdd:
expclim2[is.na(expclim2$airtemp_mean) & !is.na(expclim2$cantemp_mean) & expclim2$cantemp_mean >thresh,]$gdd_air <- expclim2[is.na(expclim2$airtemp_mean) & !is.na(expclim2$cantemp_mean) & expclim2$cantemp_mean >thresh,]$cantemp_mean-thresh
expclim2[is.na(expclim2$airtemp_mean) & !is.na(expclim2$cantemp_mean) & expclim2$cantemp_mean<=thresh,]$gdd_air <- 0
#
#now sum everything up!
expclim3<-expclim2[order(expclim2$site,expclim2$block,expclim2$plot,expclim2$year,expclim2$doy),]
cumsumnona <- function(x){cumsum(ifelse(is.na(x), 0, x)) + x*0}
countcumna <- function(x){cumsum(is.na(x))}
expclim3$cumgdd_soil<-NA
expclim3$cumgdd_soil<-ave(expclim3$gdd_soil,list(expclim3$site,expclim3$block,expclim3$plot,expclim3$year), FUN=cumsumnona)
expclim3$numnas_soilgdd<-ave(expclim3$gdd_soil,list(expclim3$site,expclim3$block,expclim3$plot,expclim3$year), FUN=countcumna)

expclim3$cumgdd_air<-NA
expclim3$cumgdd_air<-ave(expclim3$gdd_air,list(expclim3$site,expclim3$block,expclim3$plot,expclim3$year), FUN=cumsumnona)
expclim3$numnas_airgdd<-ave(expclim3$gdd_air,list(expclim3$site,expclim3$block,expclim3$plot,expclim3$year), FUN=countcumna)
#getmin.nona <- function(x){min(ifelse(is.na(x), 0, x))}
#if(!is.na(expclim3$block)){expclim3$mindoy<-ave(expclim3$doy,list(expclim3$site,expclim3$block,expclim3$plot,expclim3$year), FUN=getmin.nona)}
expclim3$mindoy<-ave(expclim3$doy,list(expclim3$site,expclim3$year), FUN=min)

###If no climate measurements taken prior to Jan 15, then exclude cumulative gdd (=NA) becuase not enough data to be reliable
expclim3[expclim3$mindoy>15,]$cumgdd_air<-NA
expclim3[expclim3$mindoy>15,]$cumgdd_soil<-NA

expclim3[expclim3$site==noblocksites[1]|expclim3$site==noblocksites[2]|expclim3$site==noblocksites[3]|expclim3$site==noblocksites[4]|expclim3$site==noblocksites[5]|expclim3$site==noblocksites[6],]$block<-NA

#Add cumulative mean soil moisture
cummean <- function(x){(cumsum(ifelse(is.na(x), 0, x)) + x*0)/seq_along(ifelse(is.na(x), 0, x))}
cummean2<-function(x){cumsum(x)/ seq_along(x)}
#expclim3<-expclim3[order(expclim3$site,expclim3$block,expclim3$plot,expclim3$year,expclim3$doy),]

#expclim3$cummean_sm<-NA
#expclim3$cummean_sm<-ave(expclim3$soilmois1,list(expclim3$site,expclim3$block,expclim3$plot,expclim3$year), FUN=cummean)
cbind(expclim3$year,expclim3$doy,expclim3$soilmois1,expclim3$cummean_sm)[30:100,]
write.csv(expclim3,"Analyses/gddchill/expclim.wchillgdd.csv", row.names=FALSE)