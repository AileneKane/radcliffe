##Plots and analyses with gdd and chilling!
##April 6, 2016
##Ailene
library(ggplot2)
##Read in exp clim and phenology files
setwd("~/GitHub/radcliffe")
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=T)
expphen<-read.csv("Analyses/exppheno.csv", head=T)
##Merge phenology with climate data file 
allexp<-merge(expclim,expphen)
head(allexp)
dim(allexp)#72283 rows
allexp$gen.sp<-paste(allexp$genus,allexp$species, sep=".")
tapply(allexp$doy,allexp$event,sum, na.rm=T)
##Start with bbd
bbd.dat<-allexp[allexp$event=="bbd",]
bbd.dat.air<-bbd.dat[!is.na(bbd.dat$cumgdd_air),]
quartz(height=6,width=8)
par(mfrow=c(1,2))
boxplot(bbd.dat$cumgdd_soil~bbd.dat$preciptreat, main="soil gdd", xlab="Precip treatment")
boxplot(bbd.dat$cumgdd_air~bbd.dat$preciptreat, main="air gdd", xlab="Precip treatment")
bbd.dat$site<-as.factor(bbd.dat$site)
#
library(lme4)
gddprecip.mod<-lmer(cumgdd_air~preciptreat+(1|site), data=bbd.dat)
summary(gddprecip.mod)
gddtemp.mod<-lmer(cumgdd_soil~-1+temptreat+(1|site/gen.sp), data=bbd.dat)
summary(gddtemp.mod)
boxplot(bbd.dat$gddmeanair~bbd.dat$temptreat, main="air gdd", xlab="Temp treatment")
#BBD plot by site
#soil gdd
quartz()
par(mfrow=c(2,3))
sites_bbd<-unique(allexp[allexp$event=="bbd",]$site)
for (i in 1:length(sites_bbd)){
  sitedat<-allexp[allexp$site==sites_bbd[i],]
  sitedat<-sitedat[!is.na(sitedat$gddmeansoil),]
  if(dim(sitedat)[1]>0){
    boxplot(sitedat$gddmeansoil~sitedat$temptreat, ylab="soil gdd", xlab="Temp treatment", main=paste(sites_bbd[i],"_bbd"))
  }
    }

#air gdd
quartz()
par(mfrow=c(2,3))
sites_bbd<-unique(allexp[allexp$event=="bbd",]$site)
for (i in 1:length(sites_bbd)){
  sitedat<-allexp[allexp$site==sites_bbd[i],]
  sitedat<-sitedat[!is.na(sitedat$cumgdd_air),]
  if(dim(sitedat)[1]>0){
    boxplot(sitedat$cumgdd_air~sitedat$temptreat, ylab="air gdd", xlab="Temp treatment", main=paste(sites_bbd[i],"_bbd"))
  }
}

#LUD plot by site
#soil gdd
quartz()
par(mfrow=c(2,3))
sites_bbd<-unique(allexp[allexp$event=="lud",]$site)
for (i in 1:length(sites_bbd)){
  sitedat<-allexp[allexp$site==sites_bbd[i],]
  sitedat<-sitedat[!is.na(sitedat$cumgdd_soil),]
  if(dim(sitedat)[1]>0){
    boxplot(sitedat$cumgdd_soil~sitedat$temptreat, ylab="soil gdd", xlab="Temp treatment", main=paste(sites_bbd[i],"_lud"))
  }
}

#air gdd
quartz()
par(mfrow=c(2,3))
sites_bbd<-unique(allexp[allexp$event=="lud",]$site)
for (i in 1:length(sites_bbd)){
  sitedat<-allexp[allexp$site==sites_bbd[i],]
  sitedat<-sitedat[!is.na(sitedat$cumgdd_air),]
  if(dim(sitedat)[1]>0){
    boxplot(sitedat$cumgdd_air~sitedat$temptreat, ylab="air gdd", xlab="Temp treatment", main=paste(sites_bbd[i],"_lud"))
  }
}
#LoD plot by site
#soil gdd
quartz()
par(mfrow=c(2,3))
sites_bbd<-unique(allexp[allexp$event=="lod",]$site)
for (i in 1:length(sites_bbd)){
  sitedat<-allexp[allexp$site==sites_bbd[i],]
  sitedat<-sitedat[!is.na(sitedat$cumgdd_soil),]
  if(dim(sitedat)[1]>0){
    boxplot(sitedat$cumgdd_soil~sitedat$temptreat, ylab="soil gdd", xlab="Temp treatment", main=paste(sites_bbd[i],"_lod"))
  }
}

#air gdd
quartz()
par(mfrow=c(2,3))
sites_bbd<-unique(allexp[allexp$event=="lod",]$site)
for (i in 1:length(sites_bbd)){
  sitedat<-allexp[allexp$site==sites_bbd[i],]
  sitedat<-sitedat[!is.na(sitedat$cumgdd_air),]
  if(dim(sitedat)[1]>0){
    boxplot(sitedat$cumgdd_air~sitedat$temptreat, ylab="air gdd", xlab="Temp treatment", main=paste(sites_bbd[i],"_lod"))
  }
}
#########Precip
quartz()
par(mfrow=c(2,3))
events<-unique(allexp$event)
for (j in 1:length(events)){
eventdat<-allexp[allexp$event==events[j],]
sites<-unique(allexp[allexp$event==events[j],]$site)
for (i in 1:length(sites)){
  sitedat<-eventdat[eventdat$site==sites[i],]
  sitedat<-sitedat[!is.na(sitedat$cumgdd_soil),]
  if(dim(sitedat)[1]>0){
    boxplot(sitedat$cumgdd_soil~sitedat$preciptreat, ylab="soil gdd", xlab="Precip treatment", main=paste(sites_bbd[i],"_",events[j]))
    } else next
}
}

#air gdd
quartz()
par(mfrow=c(2,3))
sites_bbd<-unique(allexp[allexp$event=="bbd",]$site)
for (i in 1:length(sites_bbd)){
  sitedat<-allexp[allexp$site==sites_bbd[i],]
  sitedat<-sitedat[!is.na(sitedat$cumgdd_air),]
  if(dim(sitedat)[1]>0){
    boxplot(sitedat$cumgdd_air~sitedat$preciptreat, ylab="air gdd", xlab="Precip treatment", main=paste(sites_bbd[i],"_bbd"))
  }
}

#LUD plot by site
#soil gdd
quartz()
par(mfrow=c(2,3))
sites_bbd<-unique(allexp[allexp$event=="lud",]$site)
for (i in 1:length(sites_bbd)){
  sitedat<-allexp[allexp$site==sites_bbd[i],]
  sitedat<-sitedat[!is.na(sitedat$gddmeansoil),]
  if(dim(sitedat)[1]>0){
    boxplot(sitedat$gddmeansoil~sitedat$preciptreat, ylab="soil gdd", xlab="Precip treatment", main=paste(sites_bbd[i],"_lud"))
  }
}

#air gdd
quartz()
par(mfrow=c(2,3))
sites_bbd<-unique(allexp[allexp$event=="lud",]$site)
for (i in 1:length(sites_bbd)){
  sitedat<-allexp[allexp$site==sites_bbd[i],]
  sitedat<-sitedat[!is.na(sitedat$gddmeanair),]
  if(dim(sitedat)[1]>0){
    boxplot(sitedat$gddmeanair~sitedat$preciptreat, ylab="air gdd", xlab="Precip treatment", main=paste(sites_bbd[i],"_lud"))
  }
}
#LoD plot by site
#soil gdd
quartz()
par(mfrow=c(2,3))
sites_bbd<-unique(allexp[allexp$event=="ffd",]$site)
for (i in 1:length(sites_bbd)){
  sitedat<-allexp[allexp$site==sites_bbd[i],]
  sitedat<-sitedat[!is.na(sitedat$gddmeansoil),]
  if(dim(sitedat)[1]>0){
    boxplot(sitedat$gddmeansoil~sitedat$preciptreat, ylab="soil gdd", xlab="Precip treatment", main=paste(sites_bbd[i],"_ffd"))
  }
}

#air gdd
quartz()
par(mfrow=c(2,3))
sites_bbd<-unique(allexp[allexp$event=="ffd",]$site)
for (i in 1:length(sites_bbd)){
  sitedat<-allexp[allexp$site==sites_bbd[i],]
  sitedat<-sitedat[!is.na(sitedat$gddmeanair),]
  if(dim(sitedat)[1]>0){
    boxplot(sitedat$gddmeanair~sitedat$preciptreat, ylab="air gdd", xlab="Precip treatment", main=paste(sites_bbd[i],"_ffd"))
  }
}
