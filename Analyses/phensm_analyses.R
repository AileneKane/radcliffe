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
library(dplyr)
library(AICcmodavg)
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
obspheno<-read.csv("Analyses/obspheno.csv", header=TRUE)
head(expclim)

## Fit preliminary models to get estimate of growing degree days at phenological events
## in each plot/species/site
expsites<-unique(exppheno$site)
#expsites<-expsites[-which(expsites=="exp05")]#only soil moisture data currently for this site
#exppheno2<-exppheno[-which(exppheno$site=="exp05"),]
#expsites<-expsites[-which(expsites=="exp02")]#remove for now, since temperature data is not measured within each plot
#exppheno3<-exppheno2[-which(exppheno2$site=="exp02"),]
exppheno3<-exppheno
exppheno3$genus.species<-paste(exppheno3$genus,exppheno3$species,sep=".")
#get aboveground temperature as one variable
expclim$agtemp_min<-expclim$airtemp_min
expclim[which(is.na(expclim$agtemp_min) & !is.na(expclim$cantemp_min)),]$agtemp_min<-expclim[which(is.na(expclim$airtemp_min) & !is.na(expclim$cantemp_min)),]$cantemp_min
expclim[which(is.na(expclim$agtemp_min) & !is.na(expclim$surftemp_min)),]$agtemp_min<-expclim[which(is.na(expclim$agtemp_min) & !is.na(expclim$surftemp_min)),]$surftemp_min
expclim$agtemp_max<-expclim$airtemp_max
expclim[which(is.na(expclim$agtemp_max) & !is.na(expclim$cantemp_max)),]$agtemp_max<-expclim[which(is.na(expclim$airtemp_max) & !is.na(expclim$cantemp_max)),]$cantemp_max
expclim[which(is.na(expclim$agtemp_max) & !is.na(expclim$surftemp_max)),]$agtemp_max<-expclim[which(is.na(expclim$agtemp_max) & !is.na(expclim$surftemp_max)),]$surftemp_max
#Possible approaches:
#1) Fit models to estimate gddcrit by species, plot and year. then look at GDDcrt values by moisture level?
#2) OR fit ONE model with GDDcrit as the response, species/site and year as random effects, and different combinations of warming/precip treatments check AICs
#Try 2) first, but just with sites that manipulated precip in some way. 
#exppheno4<-exppheno3[which(exppheno3$site=="exp01"|exppheno3$site=="exp02"|exppheno3$site=="exp05"|exppheno3$site=="exp09"|exppheno3$site=="exp12"),]

expall<-inner_join(exppheno3,expclim, by=c("site", "block", "plot","year","doy"), match="all")
expall$year<-as.factor(expall$year)
expall$preciptreat<-as.factor(expall$preciptreat)
expall$temptreat<-as.factor(expall$temptreat)
expall$site<-as.factor(expall$site)
expall$genus.species<-as.factor(expall$genus.species)
expall$year<-as.factor(expall$year)
expall_bbd<-expall[which(expall$event=="bbd"),]
expall_lod<-expall[expall$event=="lod",]
expall$cumgdd_air<-as.numeric(expall$cumgdd_air)
expall$doy<-as.numeric(expall$doy)

head(expall)
dim(expall)
#Jeff's questions is: So, basically, how do we know it’s the drying and not some (direct) aspect of the warming that’s causing the change?  That reasoning doesn’t come through clearly to me, and yet I think it’s the crux of the study.  A little bit of attention to that and I think it will be in good shape.
#Two get at this, try two things: look at GDDcrit in sites with warming and different levels of precip
#(exp1,5,9,12)
#First, make a figure, then fit a model:
#select out just sites that manipulate precip AND temp
expall_tp<-expall[which(expall$site=="exp01"|expall$site=="exp02"|expall$site=="exp05"|expall$site=="exp09"|expall$site=="exp12"),]#only soil moisture data currently for this site

#for exp02, the only controls are ambient controls. code these as 0s to fascilitate comparisons
expall_tp[which(expall_tp$site=="exp02" & expall_tp$temptreat=="ambient"),]$temptreat<-0

expall_tp$alltreats<-paste(expall_tp$temptreat,expall_tp$preciptreat, sep=".")
sites<-unique(expall_tp$site)

for(i in 1:length(sites)){
  sitedat<-expall_tp[expall_tp$site==sites[i],]
  quartz()
  par(mfrow=c(1,2))
  boxplot(sitedat$doy~sitedat$alltreats, main=paste(sites[i],"doy"))
  if (length(unique(sitedat$cumgdd_air))>1) {
    boxplot(sitedat$cumgdd_air~sitedat$alltreats, main=paste(sites[i],"gddcrit-air"))
  } else if(length(unique(sitedat$cumgdd_soil))>1){
    boxplot(sitedat$cumgdd_soil~sitedat$alltreats, main=paste(sites[i],"gddcrit-soil"))
  }else (next)
}
table(expall_tp$site,expall_tp$alltreats)

expall$temptreat <- relevel(as.factor( expall$temptreat), ref = "0")
expall$preciptreat <- relevel(as.factor( expall$preciptreat), ref = "0")




m1_all1<-lmer(cumgdd_air~temptreat*preciptreat + (temptreat*preciptreat|site/genus.species)+ (1|year), data=expall)
m1_all2<-lmer(cumgdd_air~temptreat*preciptreat + (1|site/genus.species)+ (1|year), data=expall)
m1_all3<-lmer(cumgdd_air~temptreat + (1|site/genus.species)+ (1|year), data=expall)
m1_all4<-lmer(cumgdd_air~temptreat + (temptreat|site/genus.species)+ (1|year), data=expall)
AIC(m1_all1,m1_all2,m1_all3,m1_all4)
aictab(list(m1_all1,m1_all2,m1_all3,m1_all4))
#all_events2 has lowest aic 
summary(all_events)
Anova(all_events)

sm_gddmod<-lmer(cumgdd_air~soilmois1 + (soilmois1|site/genus.species)+ (1|year), data=expall)
summary(sm_gddmod)
Anova(sm_gddmod)
coef(sm_gddmod)

expall$agtemp_min<-expall$airtemp_min
expall[which(is.na(expall$agtemp_min) & !is.na(expall$cantemp_min)),]$agtemp_min<-expall[which(is.na(expall$airtemp_min) & !is.na(expall$cantemp_min)),]$cantemp_min
expall[which(is.na(expall$agtemp_min) & !is.na(expall$surftemp_min)),]$agtemp_min<-expall[which(is.na(expall$agtemp_min) & !is.na(expall$surftemp_min)),]$surftemp_min
expall$agtemp_max<-expall$airtemp_max
expall[which(is.na(expall$agtemp_max) & !is.na(expall$cantemp_max)),]$agtemp_max<-expall[which(is.na(expall$airtemp_max) & !is.na(expall$cantemp_max)),]$cantemp_max
expall[which(is.na(expall$agtemp_max) & !is.na(expall$surftemp_max)),]$agtemp_max<-expall[which(is.na(expall$agtemp_max) & !is.na(expall$surftemp_max)),]$surftemp_max

sm_gddmod2<-lmer(doy~soilmois1*agtemp_max + (soilmois1*agtemp_max|site/genus.species)+ (1|year), data=expall)
summary(sm_gddmod2)
Anova(sm_gddmod2)
coef(sm_gddmod2)
sm_gddmod3<-lmer(doy~soilmois1*agtemp_max + (1|site/genus.species)+ (1|year), data=expall)
summary(sm_gddmod3)
Anova(sm_gddmod3)
coef(sm_gddmod3)
