#Preliminary analyses of exp and obs data for radcliff
#Started March 18, 2016 by Ailene Ettinger
#Modified/added to by Ailene  February 2017 for ESA abstract

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

##Read in climate and phenology data
setwd("~/git/radcliffe")
# setwd("~/Documents/git/projects/meta_ep2/radcliffe/radmeeting")

## setup
library(RColorBrewer)
library(lme4)
library(car)

expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
obspheno<-read.csv("Analyses/obspheno.csv", header=TRUE)
head(expclim)

### Now, some preliminary analyses:

## Now fit models to get estimate of growing degree days at phenological events
## in each plot/species/site
expsites<-unique(exppheno$site)
expsites<-expsites[-which(expsites=="exp05")]#only soil moisture data currently for this site
exppheno2<-exppheno[-which(exppheno$site=="exp05"),]
expsites<-expsites[-which(expsites=="exp02")]#remove for now, since temperature data is not measured within each plot
exppheno3<-exppheno2[-which(exppheno2$site=="exp02"),]
exppheno3$site<-factor(exppheno3$site)
expsites<-factor(expsites)
exppheno3$genus.species<-paste(exppheno3$genus,exppheno3$species,sep=".")

##
## the below goes through and takes averages over plots (e.g.,
# if you have three years of data for one species in one plot
# you get one value
##
allsitesgdd<-c()
for (i in 1:length(expsites)){
  phendat<-exppheno3[exppheno3$site==expsites[i],]
  climdat<-expclim[expclim$site==expsites[i],]
  expdat<-merge(climdat,phendat)
  #preciptreat<-unique(expdat$preciptreat)
  species<-unique(expdat$genus.species)
  allsppgdd<-c()
  for (j in 1:length(species)){
    spdat.allevents <- expdat[expdat$genus.species==species[j],]
    spdat.allevents$plot<-factor(spdat.allevents$plot)
    events <- unique(spdat.allevents$event)
    for (m in 1:length(events)){
       spdat <- spdat.allevents[spdat.allevents$event==events[m],]
       spdat$plot <- factor(spdat$plot)
    gdd.est<-c()
    for (k in 1:length(tbase)){
      cumgdd_soil<-spdat[,24+(k-1)+k]
      cumgdd_air<-spdat[,25+(k-1)+k] ###right now, only fitting models when there is more than one plot per species per site.
      if (length(which(!is.na(cumgdd_soil)))>0 && length(which(!is.na(tapply(cumgdd_soil,spdat$plot, mean,na.rm=TRUE))))>1){
      gdd.mod.soil<-lm(cumgdd_soil ~ -1+spdat$plot)
      est.soil <- cbind(rep(paste(expsites[i]),times=dim(coef(summary(gdd.mod.soil)))[1]),rep(species[j],                                  times=dim(coef(summary(gdd.mod.soil)))[1]), rep(events[m],times=dim(coef(summary(gdd.mod.soil)))[1]), rep("soilgdd",times=dim(coef(summary(gdd.mod.soil)))[1]),rep(tbase[k],times=dim(coef(summary(gdd.mod.soil)))[1]),round(coef(summary(gdd.mod.soil))[,1:2],digits=3),rep(round(AIC(gdd.mod.soil), digits=3), times=dim(coef(summary(gdd.mod.soil)))[1]))
  }
      else {est.soil<-c(paste(expsites[i]),species[j],"soilgdd",tbase[k],NA, NA, NA, NA)}
      if (length(which(!is.na(cumgdd_air)))>0 && length(which(!is.na(tapply(cumgdd_air,spdat$plot, mean,na.rm=TRUE))))>1){
        gdd.mod.air<-lm(cumgdd_air ~ -1+spdat$plot)
        est.air<-cbind(rep(paste(expsites[i]),times=dim(coef(summary(gdd.mod.air)))[1]),rep(species[j],times=dim(coef(summary(gdd.mod.air)))[1]),rep(events[m],times=dim(coef(summary(gdd.mod.air)))[1]), rep("airgdd",times=dim(coef(summary(gdd.mod.air)))[1]),rep(tbase[k],times=dim(coef(summary(gdd.mod.air)))[1]),round(coef(summary(gdd.mod.air))[,1:2],digits=3),rep(round(AIC(gdd.mod.air), digits=3), times=dim(coef(summary(gdd.mod.air)))[1]))
      }
      
      else {est.air<-c(paste(expsites[i]),species[j],"airgdd",tbase[k],NA,NA,NA, NA)}
      gdd.est<-rbind(gdd.est,est.soil,est.air)
  }
      }
      allsppgdd<-rbind(allsppgdd,gdd.est)
    }
    allsitesgdd<-rbind(allsitesgdd,allsppgdd)
  }

colnames(allsitesgdd)<-c("site","species","event","temptype","tbase","gdd.est","gdd.est.se","modaic")
allsitesgdd1<-as.data.frame(allsitesgdd)
  allsitesgdd1$plot<-substr(rownames(allsitesgdd1),11,13)
  rownames(allsitesgdd1)<-NULL
  allsitesgdd2<-allsitesgdd1[-which(is.na(allsitesgdd1$gdd.est)),]
  #unique(allsitesgdd1$plot)
  dim(allsitesgdd2)#19120     8
  #Add columns for temp treatment and precip treatment
treats<-read.csv("treats.csv", header=TRUE)
allsitesgdd3<-merge(allsitesgdd2,treats)
write.csv(allsitesgdd3,"gddest.csv", row.names=FALSE)
head(allsitesgdd3)
allsitesgdd3$gdd.est<-as.numeric(allsitesgdd3$gdd.est)
boxplot(as.numeric(allsitesgdd3$gdd.est)~as.factor(allsitesgdd3$temptreat))
mod<-lmer(gdd.est~temptreat+ (1|site/species), data=allsitesgdd3)


#choose  best-fit model (based on lowest AIC) for each species/site sens_gddsoil_best<-aggregate(x=allsitesens1$sens_gddsoil, by=list(allsitesens1$site,allsitesens1$species), FUN=min, na.rm=F)
##################################################################
###Old code to get effect of GDD (slope)- the code below doesn't quite work!!!
##Now fit models to get climate sensitivity (shift in phenological event per degree day)
  expsites<-unique(exppheno$site)
  expsites<-expsites[-which(expsites=="cleland")]#only soil moisture data currently
  expsites<-expsites[-which(expsites=="chuine")]#reomve for now, as i have quesitons for isabelle
  
  exppheno$genus.species<-paste(exppheno$genus,exppheno$species,sep=".")
  
  allsitesens<-c()
for (i in 1:length(expsites)){
  phendat<-exppheno[exppheno$site==expsites[i],]
  climdat<-expclim[expclim$site==expsites[i],]
  expdat<-merge(climdat,phendat)
  #preciptreat<-unique(expdat$preciptreat)
  species<-unique(expdat$genus.species)
  all.spp.sens<-c()
  for (j in 1:length(species)){
    spdat<-expdat[expdat$genus.species==species[j],]
    all.tbase.sens<-c()
    if(dim(spdat)[1]<3) next
    all.tbase.sens<-matrix(NA,nrow=length(tbase),ncol=13)
    for (k in 1:length(tbase)){
    cumgdd_soil<-spdat[,24+(k-1)+k]
    cumgdd_air<-spdat[,25+(k-1)+k]
    sens.soil<-c(NA,NA,NA,NA)
    sens.air<-c(NA,NA,NA,NA)
    aic.soil<-NA
    aic.air<-NA
    if (length(which(!is.na(cumgdd_soil)))>0) {gdd.mod.soil<-lm(spdat$doy ~ cumgdd_soil)
    if (dim(coef(summary(gdd.mod.soil)))[1]==2) {sens.soil<-coef(summary(gdd.mod.soil))[2,]} else (sens.soil<-c(NA,NA,NA,NA))
    if (length(which(!is.na(cumgdd_soil)))>0) {aic.soil<-AIC(gdd.mod.soil)}
    
    if (length(which(!is.na(cumgdd_air)))>0){gdd.mod.air<-lm(spdat$doy ~ cumgdd_air)
    sens.air<-coef(summary(gdd.mod.air))[2,]
    aic.air<-AIC(gdd.mod.air)} # else (aic.air<-NA);} else (sens.air<-c(NA,NA,NA,NA)) if (length(which(!is.na(cumgdd_air)))>0) {
    all.tbase.sens[k,]<-c(paste(expsites[i]),paste(species[j]),paste(tbase[k]),round(sens.soil, digits=3),round(aic.soil, digits=3),round(sens.air, digits=3),round(aic.air, digits=3))
    }
    all.spp.sens<-rbind(all.spp.sens,all.tbase.sens)
  }
  allsitesens<-rbind(allsitesens,all.spp.sens)
}

colnames(allsitesens)<-c("site","species","tbase","sens_gddsoil","se_soil","t_soil","p_soil","aic_gddsoil","sens_gddair","se_air","t_air","p_air","aic_gddair")
rownames(allsitesens)<-NULL
write.csv(allsitesens,"gddsens.csv" )
head(allsitesens)
allsitesens1<-as.data.frame(allsitesens)
allsitesens1$aic_gddsoil<-as.numeric(allsitesens1$aic_gddsoil)
allsitesens1$aic_gddair<-as.numeric(allsitesens1$aic_gddair)
head(allsitesens1)
sens_gddsoil_best<-aggregate(x=allsitesens1$sens_gddsoil, by=list(allsitesens1$site,allsitesens1$species), FUN=min, na.rm=F)
sens_gddair_best<-aggregate(x=allsitesens1$sens_gddair, by=list(allsitesens1$site,allsitesens1$species), FUN=min,na.rm=F)
colnames(sens_gddsoil_best)<-c("site","species","aic_gddsoil")
colnames(sens_gddair_best)<-c("site","species","aic_gddair")

sens_gdd<-merge(sens_gddsoil_best,sens_gddair_best)
colnames(sens_gdd)<-c("site","species","aic_gddsoil","aic_gddair")

boxplot(allsitesens1$sens_gddair~allsitesens1$site)
boxplot(allsitesens1$sens_gddsoil~allsitesens1$site)
allsitesens1[allsitesens1$site=="sherry",]

exp.mod<-lmer(doy ~ soiltemp1_mean + (1|site/spgen), data=expdat)
head(spdat)
