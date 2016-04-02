###Preliminary analyses of exp and obs data for radcliff
##Started March 18, 2016
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
#first, add gdd to expclim file:
###Add GDD and cumulative gdd: soiltemp-tbase, cumulative GDD for that year (sum up to that date)
expclim<-expclim[order(expclim$site,expclim$plot,expclim$year, expclim$doy),]

tbase<-c(2,4,6,8,10)
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
aggregate(expclim[, 15:24], list(expclim$site), mean, na.rm=T)
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

##Now fit models to get estimate of growing degree days at phenological events in each plot/species/site
expsites<-unique(exppheno$site)
expsites<-expsites[-which(expsites=="cleland")]#only soil moisture data currently
expsites<-expsites[-which(expsites=="chuine")]#remove for now, as i have quesitons for isabelle

exppheno$genus.species<-paste(exppheno$genus,exppheno$species,sep=".")
allsitesgdd<-c()
for (i in 1:length(expsites)){
  phendat<-exppheno[exppheno$site==expsites[i],]
  climdat<-expclim[expclim$site==expsites[i],]
  expdat<-merge(climdat,phendat)
  #preciptreat<-unique(expdat$preciptreat)
  species<-unique(expdat$genus.species)
  allsppgdd<-c()
  for (j in 1:length(species)){
    spdat<-expdat[expdat$genus.species==species[j],]
    spdat$plot<-factor(spdat$plot)
    gdd.est<-c()
    for (k in 1:length(tbase)){
      cumgdd_soil<-spdat[,24+(k-1)+k]
      cumgdd_air<-spdat[,25+(k-1)+k]###right now, only fitting models when there is more than one plot per species per site.
      if (length(which(!is.na(cumgdd_soil)))>0 && length(unique(spdat$plot))>1){
      gdd.mod.soil<-lm(cumgdd_soil ~ -1+spdat$plot)
      est.soil<-cbind(rep(expsites[i],times=dim(coef(summary(gdd.mod.soil)))[1]),rep(species[j],times=dim(coef(summary(gdd.mod.soil)))[1]),rep("soilgdd",times=dim(coef(summary(gdd.mod.soil)))[1]),rep(tbase[k],times=dim(coef(summary(gdd.mod.soil)))[1]),round(coef(summary(gdd.mod.soil))[,1:2],digits=3),rep(round(AIC(gdd.mod.soil), digits=3), times=dim(coef(summary(gdd.mod.soil)))[1]))
      } else {est.soil<-c(expsites[i],species[j],"soilgdd",tbase[k],NA,NA,NA)}
      if (length(which(!is.na(cumgdd_air)))>0 && length(which(!is.na(tapply(cumgdd_air,spdat$plot, mean,na.rm=T))))>1){
        gdd.mod.air<-lm(cumgdd_air ~ -1+spdat$plot)
        est.air<-cbind(rep(expsites[i],times=dim(coef(summary(gdd.mod.air)))[1]),rep(species[j],times=dim(coef(summary(gdd.mod.air)))[1]),rep("airgdd",times=dim(coef(summary(gdd.mod.air)))[1]),rep(tbase[k],times=dim(coef(summary(gdd.mod.air)))[1]),round(coef(summary(gdd.mod.air))[,1:2],digits=3),rep(round(AIC(gdd.mod.air), digits=3), times=dim(coef(summary(gdd.mod.air)))[1]))
      } else {est.air<-c(expsites[i],species[j],"airgdd",tbase[k],NA,NA,NA)}
      gdd.est<-rbind(gdd.est,est.soil,est.air)
      }
      allsppgdd<-rbind(allsppgdd,gdd.est)
    }
    allsitesgdd<-rbind(allsitesgdd,allsppgdd)
  }
  colnames(allsitesgdd)<-c("site","species","temptype","tbase","gdd.est","gdd.est.se","modaic")
  allsitesgdd1<-as.data.frame(allsitesgdd)
  allsitesgdd1$plot<-substr(rownames(allsitesgdd1),11,13)
  rownames(allsitesgdd1)<-NULL
  allsitesgdd2<-allsitesgdd1[-which(is.na(allsitesgdd1$gdd.est)),]
  #unique(allsitesgdd1$plot)
  write.csv(allsitesgdd2,"gddest.csv" )

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
