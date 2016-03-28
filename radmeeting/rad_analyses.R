##Preliminary analyses of exp and obs data for radcliff
##March 18, 2016
##Read in climate and phenology data
setwd("~/GitHub/radcliffe/radmeeting")
#obsclim<-read.csv("obsclim.csv", header=T)
expclim<-read.csv("expclim.csv", header=T)
exppheno<-read.csv("exppheno.csv", header=T)
obspheno<-read.csv("obspheno.csv", header=T)
head(expclim)
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

#first, add gdd to expclim file:
###Add GDD to all: soiltemp-tbase (=10), cumulative GDD for that year (sum up to that date)
tbase<-10
expclim$gdd_soil<-expclim$soiltemp1_mean-tbase
expclim$gdd_air<-((as.numeric(expclim$airtemp_min)+as.numeric(expclim$airtemp_max))/2)-tbase
expclim$gdd_soil[expclim$gdd_soil < 10] <- 0
expclim$gdd_air[expclim$gdd_air < 10] <- 0
expclim<-expclim[order(expclim$site,expclim$plot,expclim$year, expclim$doy),]
cumsumnona <- function(x){cumsum(ifelse(is.na(x), 0, x)) + x*0}
expclim$cumgdd_air<- ave(expclim$gdd_air,list(expclim$site,expclim$plot,expclim$year), FUN=cumsumnona)
expclim$cumgdd_soil <- ave(expclim$gdd_soil,list(expclim$site,expclim$plot,expclim$year), FUN=cumsumnona)

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
  if (length(which(!is.na(spdat$cumgdd_air)))>0){gdd.mod.air<-lm(doy ~ cumgdd_air, data=spdat)
  sens.air<-coef(gdd.mod.air)[2]} else (sens.air<-NA)
  dat.sens<-c(paste(expsites[i]),paste(species[j]),round(sens.soil, digits=3),round(sens.air, digits=3))
  plot()
  all.spp.sens<-rbind(all.spp.sens,dat.sens)
  }
allsitesens<-rbind(allsitesens,all.spp.sens)
}

