###Preliminary analyses of exp and obs data for radcliff
##STarted March 18, 2016
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
for (i in 1:length(tbase)){
  expclim[,24+(i-1)+i]<-expclim$soiltemp1_mean-tbase[i]
  expclim[,25+(i-1)+i]<-((as.numeric(expclim$airtemp_min)+as.numeric(expclim$airtemp_max))/2)-tbase[i]
  expclim[,24+(i-1)+i][expclim[24+i] < tbase[i]] <- 0
  expclim[,25+(i-1)+i][expclim[25+i] < tbase[i]] <- 0
}
for (i in 1:length(tbase)){
  colnames(expclim)[14+(i-1)+i]<-paste("gdd_soil",tbase[i],sep=".")
  colnames(expclim)[15+(i-1)+i]<-paste("gdd_air",tbase[i],sep=".")
}
cumsumnona <- function(x){cumsum(ifelse(is.na(x), 0, x)) + x*0}
expclim$cumgdd_air<- ave(expclim$gdd_air,list(expclim$site,expclim$plot,expclim$year), FUN=cumsumnona)
expclim$cumgdd_soil <- ave(expclim$gdd_soil,list(expclim$site,expclim$plot,expclim$year), FUN=cumsumnona)
expclim$alltreat<-paste(expclim$temptreat,expclim$preciptreat,sep=".")
#Generate GDDs using 5 different base temps (2,4,6,8,10)




#Look at how treatment affects growing degree days
quartz(height=6,width=10)
par(mfrow=c(2,1))
boxplot(expclim$cumgdd_air~expclim$alltreat)
boxplot(expclim$cumgdd_soil~expclim$alltreat)
#Try fitting a model to look at this:
#remove sham and outside treatments for now
expclim2<-expclim[!expclim$temptreat=="sham",]
expclim2<-expclim2[!expclim2$temptreat=="outside",]
temptreat.mod<-lmer(cumgdd_air~doy*temptreat+(1|site),data=expclim2)
temptreat.mod2<-lmer(cumgdd_air~doy+temptreat+(1|site),data=expclim2)
temptreat.mod3<-lmer(cumgdd_air~doy+(1|site),data=expclim2)
AIC(temptreat.mod,temptreat.mod2,temptreat.mod3)

##Now fit models to get climate sensitivity (shift in phenological event per degree)
expsites<-unique(exppheno$site)
allsitesens<-NA
for (i in 1:length(expsites)){
  phendat<-exppheno[exppheno$site==expsites[i],]
  climdat<-expclim[expclim$site==expsites[i],]
  expdat<-merge(climdat,phendat)
  #preciptreat<-unique(expdat$preciptreat)
  species<-unique(expdat$genus.species)
  all.spp.sens<-NA
  for (j in 1:length(species)){
    spdat<-expdat[expdat$genus.species==species[j],]
    if (length(which(!is.na(spdat$cumgdd_soil)))>0) {gdd.mod.soil<-lm(doy ~ cumgdd_soil, data=spdat)
    sens.soil<-coef(gdd.mod.soil)[2]} else (sens.soil<-NA)
    print(expsites[i])
    print(species[j])
    print(summary(gdd.mod.soil))
    if (length(which(!is.na(spdat$cumgdd_air)))>0){gdd.mod.air<-lm(doy ~ cumgdd_air, data=spdat)
    sens.air<-coef(gdd.mod.air)[2]} else (sens.air<-NA)
    print(summary(gdd.mod.air))
    dat.sens<-c(paste(expsites[i]),paste(species[j]),round(sens.soil, digits=3),round(sens.air, digits=3))
    #plot()
    all.spp.sens<-rbind(all.spp.sens,dat.sens)
  }
  allsitesens<-rbind(allsitesens,all.spp.sens)
}

colnames(allsitesens)<-c("site","species","sens_gddsoil","sens_gddair")
rownames(allsitesens)<-NULL
allsitesens1<-as.data.frame(allsitesens[-which(is.na(allsitesens[,1])),])
allsitesens1$sens_gddsoil<-as.numeric(allsitesens1$sens_gddsoil)
allsitesens1$sens_gddair<-as.numeric(allsitesens1$sens_gddair)
head(allsitesens1)
boxplot(allsitesens1$sens_gddair~allsitesens1$site)
boxplot(allsitesens1$sens_gddsoil~allsitesens1$site)
allsitesens1[allsitesens1$site=="sherry",]


##get species lists with sites for obs and exp data
expsites<-unique(exppheno$site)
allsitespp<-NA
for (i in 1:length(expsites)){
  sitedat<-exppheno[exppheno$site==expsites[i],]
  specieslist<-sort(unique(paste(sitedat$genus,sitedat$species, sep=".")))
  sitespp<-cbind(rep(expsites[i], times=length(specieslist)),specieslist)
  allsitespp<-rbind(allsitespp,sitespp)
}
write.csv(allsitespp,"exp_splist_Site.csv")
obssites<-unique(obspheno$site)

allsitespp<-NA
for (i in 1:length(obssites)){
  sitedat<-obspheno[obspheno$site==obssites[i],]
  specieslist<-sort(unique(paste(sitedat$genus,sitedat$species, sep=".")))
  sitespp<-cbind(rep(obssites[i], times=length(specieslist)),specieslist)
  allsitespp<-rbind(allsitespp,sitespp)
}
write.csv(allsitespp,"obs_splist_Site.csv")
