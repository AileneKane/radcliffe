#Preliminary analyses of experimental phenology, soil moisture, and temperature data for radcliffe
#Started March 18, 2016 by Ailene Ettinger
#Modified/added to by Ailene February-April 2017 for ESA abstract and additional analyses
#Three questions to address:
#1) How do warming and precip treatments affect soil moisture? (Make plots and fit models)
#2) How does soil moisture affect GDDcrit?
#3) How do soil moisture and temperature affect doy of bud burst, leaf out, etc?

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
## load packages
library(RColorBrewer)
library(lme4)
library(car)
library(dplyr)
library(AICcmodavg)
#update.packages()

#Read in experimental climate and phenology data
setwd("~/git/radcliffe")
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)

#standard data wrangling to get expclim2 for climate analyses and  expgdd for phenology analyses (with gddcrit)
source("analyses/source/standard_mergesandwrangling.R")

#summarize climate data by plot (annual and seasonal temp, soil mois) and merge in with expgdd file
source("analyses/source/climsum_byplot.R")

expclim2$target_cent<-scale(expclim2$target, center = TRUE, scale = TRUE)
expclim2$preciptreat_amt_cent<-scale(expclim2$preciptreat_amt, center = TRUE, scale = TRUE)
expclim2a<-subset(expclim2,select=c(site,year,doy,target_cent,preciptreat_amt,target,preciptreat_amt_cent,soilmois1))
expclim2a<- expclim2a [apply(expclim2a , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#First, 
#1) How do warming and precip treatments affect soil moisture? (Make plots and fit models)

#fit model in lmer
###Fit lmer model for soil moisture~warming*preciptreatment
#sm_mod_cent<-lmer(soilmois1~target_cent*preciptreat_amt_cent + (1|site/year/doy), REML=FALSE, data=expclim2a)
#summary(sm_mod_cent)
#sm_mod_cent2<-lmer(soilmois1~target_cent*preciptreat_amt_cent + (1|site)+ (1|year/doy), REML=FALSE, data=expclim2a)
#summary(sm_mod_cent2)#fixed coef results almost identical to other random effects structure and i think nested time effects makes more sense given how far apart sites arestructure 

sm_mod<-lmer(soilmois1~target*preciptreat_amt + (1|site/year/doy), REML=FALSE, data=expclim2a)
summary(sm_mod)

sm_tempmod<-lmer(soilmois1~target + (1|site/year/doy), REML=FALSE, data=expclim2a)
summary(sm_tempmod)

#Figure with all raw points
#quartz(height=5, width=9)
#par(mfrow=c(1,2))
#plot(expclim2$target,expclim2$soilmois1,type="p", pch=21,bg="lightgray",col="gray", ylab="Soil moisture", xlab="Target warming (C)", bty="l", ylim=c(0,0.8))
#abline(a=fixef(sm_mod)[1],b=fixef(sm_mod)[2], lwd=2)
#temp_slope=round(fixef(sm_mod)[2], digits=4)
#temp_se=round(summary(sm_mod)$coefficients[2,2], digits=3)
#mtext(paste("coef=",temp_slope), side=3, line=-4, adj=.8)

#plot(expclim2$preciptreat_amt,expclim2$soilmois1,type="p", pch=21,bg="lightgray",col="gray", ylab="Soil moisture", xlab="Precipitation treatment (%)", bty="l", ylim=c(0,0.8))
#abline(a=fixef(sm_mod)[1],b=fixef(sm_mod)[3], lwd=2)
#prec_slope=round(fixef(sm_mod)[3], digits=4)
#prec_se=round(summary(sm_mod)$coefficients[3,3], digits=4)
#mtext(paste("coef=",prec_slope), side=3, line=-4, adj=.8)

#Make the above figure but without raw data- just site differences (random effects) plotted
#plot fitted lines, points, and 1:1 line
sites<-unique(expclim2$site)
expclim2$target<-as.numeric(expclim2$target)
soilmois_target<-aggregate(expclim2$soilmois1, by=list(expclim2$site,expclim2$target), FUN=mean,na.rm=TRUE)
soilmois_precip<-aggregate(expclim2$soilmois1, by=list(expclim2$site,expclim2$preciptreat_amt), FUN=mean,na.rm=TRUE)
colnames(soilmois_target)<-c("site","target","soilmois1")
colnames(soilmois_precip)<-c("site","precip_amt","soilmois1")
soilmois_precip<-soilmois_precip[!soilmois_precip$site=="exp06"|soilmois_precip$site=="exp11",]#no moisture data for these 2 sites
soilmois_target<-soilmois_target[!soilmois_target$site=="exp06"|soilmois_target$site=="exp11",]

cols <- brewer.pal(10,"Spectral")
#cols <- colorRampPalette(brewer.pal(11,"Spectral"))(12)#

quartz(height=5, width=9)
par(mfrow=c(1,2))
plot(soilmois_target$target,soilmois_target$soilmois1,type="p",bg=cols[as.numeric(as.factor(soilmois_target$site))], pch=21,xlab="Target warming (C)", ylab="Soil moisture (VWC)", bty="l", xlim=c(0,6), ylim=c(0,0.4))
abline(a=fixef(sm_mod)[1],b=fixef(sm_mod)[2], lwd=2)
temp_slope=round(fixef(sm_mod)[2], digits=4)
temp_se=round(summary(sm_mod)$coefficients[2,2], digits=3)
#mtext(paste("coef=",temp_slope), side=3, line=-4, adj=.8)

plot(soilmois_precip$precip,soilmois_precip$soilmois1,type="p",bg=cols[as.numeric(as.factor(soilmois_target$site))], pch=21,xlab="Precipitation treatment (%)", ylab="Soil moisture (VWC)", bty="l", xlim=c(0,200), ylim=c(0,0.4))
abline(a=fixef(sm_mod)[1],b=fixef(sm_mod)[3], lwd=2)
prec_slope=round(fixef(sm_mod)[3], digits=4)
prec_se=round(summary(sm_mod)$coefficients[3,3], digits=4)
#mtext(paste("coef=",prec_slope), side=3, line=-4, adj=.8)
legend(158, .19,legend=rownames(ranef(sm_mod)$site),pch=21, pt.bg=cols[as.numeric(as.factor(rownames(ranef(sm_mod)$site)))], cex=0.6)

#Analyses started April 11, 2017
#Want to fit a model with soil moisture and above-ground temperature as predictors for doy of phenological event
#Start by looking at which studies have both SM and AG temp data
#which(tapply(expclim2$agtemp_mn,expclim2$site,mean,na.rm=T)>0)
#which(tapply(expclim2$soilmois1,expclim2$site,mean,na.rm=T)>0)
#The following sites have both: exp01 exp02 exp03 exp04 exp07 exp09 exp10 exp12 
expgdd_subs<-expgdd4[which(expgdd4$site=="exp01"|expgdd4$site=="exp02"|expgdd4$site=="exp03"|expgdd4$site=="exp04"|expgdd4$site=="exp07"|expgdd4$site=="exp09"|expgdd4$site=="exp10"|expgdd4$site=="exp12"),]#
expgdd_subs<-subset(expgdd_subs,select=c(site,block, plot,year,target,preciptreat_amt,agtmax,agtmin,sm,doy,genus.species,event,cumgdd_air,ag_min_janmar,soilmois_janmar,ag_min_aprjun,soilmois_aprjun))
expgdd_subs <- expgdd_subs[apply(expgdd_subs, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#scale variables:
expgdd_subs$sm_cent<-scale(expgdd_subs$sm, center = TRUE, scale = TRUE)
expgdd_subs$smjm_cent<-scale(expgdd_subs$soilmois_janmar, center = TRUE, scale = TRUE)
expgdd_subs$smaj_cent<-scale(expgdd_subs$soilmois_aprjun, center = TRUE, scale = TRUE)
expgdd_subs$agtmin_cent<-scale(expgdd_subs$agtmin, center = TRUE, scale = TRUE)
expgdd_subs$agtmax_cent<-scale(expgdd_subs$agtmax, center = TRUE, scale = TRUE)
expgdd_subs$agtminjm_cent<-scale(expgdd_subs$ag_min_janmar, center = TRUE, scale = TRUE)
expgdd_subs$agtminaj_cent<-scale(expgdd_subs$ag_min_aprjun, center = TRUE, scale = TRUE)

#Figure out the random effects structure (random slopes vs intecepts:
sm_doymod<-lmer(doy~agtmax_cent*sm_cent + (1|genus.species)+ (1|site/year), REML=TRUE, data=expgdd_subs)
sm_doymod2<-lmer(doy~agtmax_cent*sm_cent + (agtmax_cent*sm_cent|genus.species)+ (1|site/year), REML=TRUE, data=expgdd_subs, control=lmerControl(optCtrl=list(maxfun=20000)))
#sm_doymod2 does not converge- something to try in stan!

#Look at trends in seasonal values for soil moisture across all phenological events
#sm_doymod<-lmer(doy~agtmax_cent*smjm_cent + (1|genus.species)+ (1|site/year), REML=TRUE, data=expgdd_subs)
#sm_doymod<-lmer(doy~agtmax_cent*smaj_cent + (1|genus.species)+ (1|site/year), REML=TRUE, data=expgdd_subs)

#uncentered explanatory variables
sm_doymod2<-lmer(doy~agtmax*sm + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_subs)

summary(sm_doymod2)#-4.4574=coefficient of uncentered model
temp_doymod2<-lmer(doy~agtmax + (1|genus.species)+ (1|site/year), REML=TRUE, data=expgdd_subs)
summary(temp_doymod2)#-2.27773=coefficient for temp of uncentered model

#AIC(temp_doymod2,sm_doymod2)#sm mod wins,  wins by 210 AIC units.

#expgdd_leaf<-expgdd_subs[which(expgdd_subs$event=="bbd"|expgdd_subs$event=="lod"|expgdd_subs$event=="lod"|expgdd_subs$event=="fgn"|expgdd_subs$event=="fnb"|expgdd_subs$event=="fyn"),]#
#expgdd_flow<-expgdd_subs[which(expgdd_subs$event=="ffd"),]#
#sm_doymod2_leaf<-lmer(doy~agtmax*sm + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_leaf)
#sm_doymod2_flow<-lmer(doy~agtmax*sm + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_flow)
#summary(sm_doymod2_leaf)#-4.5472=coefficient for temp of leaf events
#summary(sm_doymod2_flow)#3.1448=coefficient for temp of flow events
#temp_doymod2_leaf<-lmer(doy~agtmax + (1|genus.species)+ (1|site/year), REML=TRUE, data=expgdd_leaf)
#temp_doymod2_flow<-lmer(doy~agtmax + (1|genus.species)+ (1|site/year), REML=TRUE, data=expgdd_flow)
#summary(temp_doymod2_leaf)#2.609=coef of temp only of leaf events
#summary(temp_doymod2_flow)#0.1427=coef of temp only of flow events
#sm_doymodA<-lmer(doy~agtmax_cent + (agtmax_cent|site/genus.species)+ (1|year), REML=FALSE, data=expgdd_subs)
#sm_doymodS<-lmer(doy~sm_cent + (sm_cent|site/genus.species)+ (1|year), REML=FALSE, data=expgdd_subs)
#sm_doymodAS<-lmer(doy~agtmax_cent+sm_cent + (agtmax_cent+sm_cent|site/genus.species)+ (1|year), REML=FALSE, data=expgdd_subs)
#AIC(sm_doymod2,sm_doymodA,sm_doymodS,sm_doymodAS)#mod2 wins, so this is our working model
summary(sm_doymod)

#I think it only makes sense to fit models for particular events- start with bbd, lod and lud since these are the most common.
expgdd_bbd<-expgdd_subs[which(expgdd_subs$event=="bbd"),]#
expgdd_bbd$sm_cent<-scale(expgdd_bbd$sm, center = TRUE, scale = TRUE)
expgdd_bbd$smjm_cent<-scale(expgdd_bbd$soilmois_janmar, center = TRUE, scale = TRUE)
expgdd_bbd$ag_min_jm_cent<-scale(expgdd_bbd$ag_min_janmar, center = TRUE, scale = TRUE)
#expgdd_bbd$agtmax_cent<-scale(expgdd_bbd$agtmax, center = TRUE, scale = TRUE)

expgdd_lod<-expgdd_subs[which(expgdd_subs$event=="lod"),]#
expgdd_lod$sm_cent<-scale(expgdd_lod$sm, center = TRUE, scale = TRUE)
expgdd_lod$agtmin_cent<-scale(expgdd_lod$agtmin, center = TRUE, scale = TRUE)
expgdd_lod$agtmax_cent<-scale(expgdd_lod$agtmax, center = TRUE, scale = TRUE)
expgdd_lod$smaj_cent<-scale(expgdd_lod$soilmois_aprjun, center = TRUE, scale = TRUE)
expgdd_lod$ag_min_aj_cent<-scale(expgdd_lod$ag_min_aprjun, center = TRUE, scale = TRUE)


expgdd_lud<-expgdd_subs[which(expgdd_subs$event=="lud"),]#
expgdd_lud$sm_cent<-scale(expgdd_lud$sm, center = TRUE, scale = TRUE)
expgdd_lud$agtmin_cent<-scale(expgdd_lud$agtmin, center = TRUE, scale = TRUE)
expgdd_lud$agtmax_cent<-scale(expgdd_lud$agtmax, center = TRUE, scale = TRUE)
expgdd_lud$smaj_cent<-scale(expgdd_lud$soilmois_aprjun, center = TRUE, scale = TRUE)
expgdd_lud$ag_min_aj_cent<-scale(expgdd_lud$ag_min_aprjun, center = TRUE, scale = TRUE)


#smbbdmod<-lmer(doy~agtmax_cent*sm_cent + (1|genus.species)+ (1|site/year)+ (1|year), REML=FALSE, data=expgdd_bbd)
smbbdmod_cent<-lmer(doy~ag_min_jm_cent*smjm_cent + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_bbd)
smbbdmod<-lmer(doy~ag_min_janmar*soilmois_janmar + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_bbd)

#summary(smbbdmod)
summary(smbbdmod)
smlodmod_cent<-lmer(doy~ag_min_aj_cent*smaj_cent + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_lod)
smlodmod<-lmer(doy~ag_min_aprjun*soilmois_aprjun + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_lod)
summary(smlodmod)

smludmod_cent<-lmer(doy~ag_min_aj_cent*smaj_cent + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_lud)
smludmod<-lmer(doy~ag_min_aprjun*soilmois_aprjun + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_lud)

summary(smludmod)
#What about flowering?
expgdd_ffd<-expgdd_subs[which(expgdd_subs$event=="ffd"),]#
min(expgdd_ffd$doy); max(expgdd_ffd$doy);mean(expgdd_ffd$doy)
min(expgdd_ffd$ag_min_aprjun); max(expgdd_ffd$ag_min_aprjun);mean(expgdd_ffd$ag_min_aprjun)

#mean agmin-13.01533
smffdmod<-lmer(doy~ag_min_aprjun*soilmois_aprjun + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_ffd)
summary(smffdmod)

#now bbd and ffd for just matching sites (exp01 and exp10)
expgdd_bbdexp0110<-expgdd_bbd[which(expgdd_bbd$site=="exp01"|expgdd_bbd$site=="exp10"),]#
expgdd_ffddexp0110<-expgdd_ffd[which(expgdd_ffd$site=="exp01"|expgdd_ffd$site=="exp10"),]#
smffdmod<-lmer(doy~ag_min_aprjun*soilmois_aprjun + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_ffddexp0110)
smbbdmod<-lmer(doy~ag_min_janmar*soilmois_janmar + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_bbdexp0110)
summary(smffdmod)
summary(smbbdmod)


#What about senescence? only one site measured sensence
expgdd_sen<-expgdd_subs[which(expgdd_subs$event=="sen"),]#
smsenmod<-lmer(doy~ag_min_aprjun*soilmois_aprjun + (1|genus.species), REML=FALSE, data=expgdd_sen)
summary(smsenmod)#soil moisture has minimal effect on senescence
#What about leaf drop? only one site measured sensence
expgdd_drop<-expgdd_subs[which(expgdd_subs$event=="drop"),]#
smdropmod<-lmer(doy~ag_min_aprjun*soilmois_aprjun + (1|genus.species), REML=FALSE, data=expgdd_drop)
summary(smsenmod)#soil moisture has minimal effect on senescence

#What about fruiting? only one site measured fruiting
expgdd_ffrd<-expgdd_subs[which(expgdd_subs$event=="ffrd"),]#
smffrdmod<-lmer(doy~ag_min_aprjun*soilmois_aprjun + (1|genus.species), REML=FALSE, data=expgdd_ffrd)
summary(smffrdmod)

#Make plots of these two models
cols <- brewer.pal(6,"Spectral")
##ACross all events, centered
quartz(height=5, width=9)
par(mfrow=c(1,2))
#sm_doymod<-lmer(doy~agtmax_cent*sm_cent , expgdd_subs
plot(expgdd_subs$agtmax_cent,expgdd_subs$doy,type="p",bg=cols[as.numeric(as.factor(expgdd_subs$site))], pch=21,xlab="Mean annual temperature, centered", ylab="Day of year", bty="l")
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10))
abline(a=fixef(sm_doymod)[1],b=fixef(sm_doymod)[2], lwd=2)

plot(expgdd_subs$sm_cent,expgdd_subs$doy,type="p",bg=cols[as.numeric(as.factor(expgdd_subs$site))], pch=21,xlab="Mean annual soil moisture, centered", ylab="Day of year", bty="l")
abline(a=fixef(sm_doymod)[1],b=fixef(sm_doymod)[3], lwd=2)
legend(2.5, 300,legend=unique(expgdd_subs$site),pch=21, pt.bg=cols[as.numeric(as.factor(unique(expgdd_subs$site)))], cex=0.6)

#plot with fitted lines only (no points)
quartz(height=5, width=9)
par(mfrow=c(1,2))
#sm_doymod<-lmer(doy~agtmax_cent*sm_cent , expgdd_subs
#plot(expgdd_subs$agtmax_cent,expgdd_subs$doy,type="p",col="white", pch=21,xlab="Mean annual temperature, centered", ylab="Day of year", bty="l")
plot(1,type="n",xlab="Mean annual temperature, centered", ylab="Day of year", bty="l", xlim=c(min(expgdd_subs$agtmax_cent),max(expgdd_subs$agtmax_cent)),ylim=c(50,250))

for(i in 1:dim(ranef(sm_doymod)$site)[1]){
  abline(a=coef(sm_doymod)$site[i,1],b=fixef(sm_doymod)[2], lwd=1, col=cols[i])
}
abline(a=fixef(sm_doymod)[1],b=fixef(sm_doymod)[2], lwd=2)

plot(1,type="n",xlab="Mean annual soil moisture, centered", ylab="Day of year", bty="l",xlim=c(min(expgdd_subs$sm_cent),max(expgdd_subs$sm_cent)),ylim=c(50,250))
for(i in 1:dim(ranef(sm_doymod)$site)[1]){
  abline(a=coef(sm_doymod)$site[i,1],b=fixef(sm_doymod)[3], lwd=1, col=cols[i])
}
abline(a=fixef(sm_doymod)[1],b=fixef(sm_doymod)[3], lwd=2)

legend(2.1, 250,legend=unique(expgdd_subs$site),lty=1, col=cols[as.numeric(as.factor(unique(expgdd_subs$site)))], cex=0.6)

#Uncentered plot with fitted lines only (no points)
quartz(height=5, width=9)
par(mfrow=c(1,2))
#sm_doymod<-lmer(doy~agtmax_cent*sm_cent , expgdd_subs
#plot(expgdd_subs$agtmax_cent,expgdd_subs$doy,type="p",col="white", pch=21,xlab="Mean annual temperature, centered", ylab="Day of year", bty="l")
plot(1,type="n",xlab="Mean annual temperature", ylab="Day of year", bty="l", xlim=c(0,max(expgdd_subs$agtmax)),ylim=c(50,250))

for(i in 1:dim(ranef(sm_doymod2)$site)[1]){
  abline(a=coef(sm_doymod2)$site[i,1],b=fixef(sm_doymod2)[2], lwd=1, col=cols[i])
}
abline(a=fixef(sm_doymod2)[1],b=fixef(sm_doymod2)[2], lwd=2)

plot(1,type="n",xlab="Mean annual soil moisture", ylab="Day of year", bty="l",xlim=c(0,max(expgdd_subs$sm)),ylim=c(50,250))
for(i in 1:dim(ranef(sm_doymod)$site)[1]){
  abline(a=coef(sm_doymod2)$site[i,1],b=fixef(sm_doymod2)[3], lwd=1, col=cols[i])
}
abline(a=fixef(sm_doymod2)[1],b=fixef(sm_doymod2)[3], lwd=2)

legend(.24, 110,legend=unique(expgdd_subs$site),lty=1, col=cols[as.numeric(as.factor(unique(expgdd_subs$site)))], cex=0.6)


#Uncentered plot to show interaction effect: effect of soil moisture at high and low temperatures
#with fitted lines only (no points)
quartz(height=5, width=9)
par(mfrow=c(1,2))
plot(1,type="n",xlab="Mean annual soil moisture", ylab="Day of year", bty="l",xlim=c(0,max(expgdd_subs$sm)),ylim=c(50,250))
abline(a=fixef(sm_doymod2)[1],b=fixef(sm_doymod2)[3], lwd=2, col="blue")#main effect of soil moisture, so this is when temp=0
#what about when temp=10
abline(a=fixef(sm_doymod2)[1],b=(fixef(sm_doymod2)[3]+(fixef(sm_doymod2)[4]*10)), lwd=2, col="yellow")#effect of soil moisture when temp=10
abline(a=fixef(sm_doymod2)[1],b=(fixef(sm_doymod2)[3]+(fixef(sm_doymod2)[4]*20)), lwd=2, col="red")#effect of soil moisture when temp=2 0



#BBD- uncentered, without points
quartz(height=5, width=9)
par(mfrow=c(1,2))
plot(1,type="n",xlab="Above-ground Temperature (Jan-Mar)", ylab="Day of year", bty="l", xlim=c(0,max(expgdd_bbd$agtmin)),ylim=c(20,150))

for(i in 1:dim(ranef(smbbdmod)$site)[1]){
  abline(a=coef(smbbdmod)$site[i,1],b=fixef(smbbdmod)[2], lwd=1, col=cols[i])
}
abline(a=fixef(smbbdmod)[1],b=fixef(smbbdmod)[2], lwd=2)

plot(1,type="n",xlab="Mean soil moisture (Jan-Mar)", ylab="Day of year", bty="l",xlim=c(0,max(expgdd_bbd$sm)),ylim=c(20,150))
for(i in 1:dim(ranef(smbbdmod)$site)[1]){
  abline(a=coef(smbbdmod)$site[i,1],b=fixef(smbbdmod)[3], lwd=1, col=cols[i])
}
abline(a=fixef(smbbdmod)[1],b=fixef(smbbdmod)[3], lwd=2)

legend(.24, 80,legend=unique(expgdd_bbd$site),lty=1, col=cols[as.numeric(as.factor(unique(expgdd_bbd$site)))], cex=0.6)



quartz(height=5, width=9)
par(mfrow=c(1,2))
plot(expgdd_bbd$ag_min_janmar,expgdd_bbd$doy,type="p",bg=cols[as.numeric(as.factor(expgdd_bbd$site))], pch=21,xlab="Above-ground Temperature (Jan-Mar)", ylab="Day of year", bty="l")
abline(a=fixef(smbbdmod)[1],b=fixef(smbbdmod)[2], lwd=2)
plot(expgdd_bbd$soilmois_janmar,expgdd_bbd$doy,type="p",bg=cols[as.numeric(as.factor(expgdd_bbd$site))], pch=21,xlab="Soil Moisture (Jan-Mar)", ylab="Day of year", bty="l")
abline(a=fixef(smbbdmod)[1],b=fixef(smbbdmod)[3], lwd=2)

quartz(height=5, width=9)
par(mfrow=c(1,2))
plot(1,type="n",xlab="Above-ground Temperature (Jan-Mar)", ylab="Day of year", bty="l")

for(i in 1:dim(ranef(sm_doymod)$site)[1]){
  abline(a=coef(sm_doymod)$site[i,1],b=fixef(sm_doymod)[2], lwd=1, col=cols[i])
}
abline(a=fixef(sm_doymod)[1],b=fixef(sm_doymod)[2], lwd=2)

plot(1,type="n",xlab="Mean annual soil moisture, centered", ylab="Day of year", bty="l",xlim=c(min(expgdd_subs$sm_cent),max(expgdd_subs$sm_cent)),ylim=c(min(expgdd_subs$doy),max(expgdd_subs$doy)))
for(i in 1:dim(ranef(sm_doymod)$site)[1]){
  abline(a=coef(sm_doymod)$site[i,1],b=fixef(sm_doymod)[3], lwd=1, col=cols[i])
}
abline(a=fixef(mbbdmod)[1],b=fixef(sm_doymod)[3], lwd=2)
mbbdmod

##LOD
quartz(height=5, width=9)
par(mfrow=c(1,2))
plot(expgdd_lod$ag_min_aj_cent,expgdd_lod$doy,type="p",bg=cols[as.numeric(as.factor(expgdd_lod$site))], pch=21,xlab="Above-ground Temperature (Apr-Jun), centered", ylab="Day of year", bty="l")
abline(a=fixef(smlodmod)[1],b=fixef(smlodmod)[2], lwd=2)
plot(expgdd_lod$smjm_cent,expgdd_lod$doy,type="p",bg=cols[as.numeric(as.factor(expgdd_lod$site))], pch=21,xlab="Soil Moisture (Apr-Jun), centered", ylab="Day of year", bty="l")
abline(a=fixef(smlodmod)[1],b=fixef(smlodmod)[3], lwd=2)

#Make plots of soil moisture effects on BBD and FFD:
#BBD- uncentered, without points
quartz(height=5, width=9)
par(mfrow=c(1,2))

plot(1,type="n",xlab="Mean soil moisture (Jan-Mar)", ylab="Day of year", bty="l",xlim=c(0,max(expgdd_bbd$sm)),ylim=c(20,150))
for(i in 1:dim(ranef(smbbdmod)$site)[1]){
  abline(a=coef(smbbdmod)$site[i,1],b=fixef(smbbdmod)[3], lwd=1, col=cols[i])
}
abline(a=fixef(smbbdmod)[1],b=fixef(smbbdmod)[3], lwd=2)
legend(.24, 60,legend=unique(expgdd_bbd$site),lty=1, col=cols[as.numeric(as.factor(unique(expgdd_bbd$site)))], cex=0.6)

#Flowering
plot(1,type="n",xlab="Mean soil moisture (Apr-Jun)", ylab="Day of year", bty="l",xlim=c(0,max(expgdd_ffd$sm)),ylim=c(100,230))
for(i in 1:dim(ranef(smffdmod)$site)[1]){
  abline(a=coef(smffdmod)$site[i,1],b=fixef(smffdmod)[3], lwd=1, col=cols[i])
}
abline(a=fixef(smffdmod)[1],b=fixef(smffdmod)[3], lwd=2)

legend(.24, 140,legend=unique(expgdd_ffd$site),lty=1, col=cols[as.numeric(as.factor(unique(expgdd_ffd$site)))], cex=0.6)





#Make plot, showing random effects for each site
#Budburst data
sitesbb<-unique(expgdd_bbd$site)
soilmois_sitsbb<-aggregate(expgdd_bbd$smjm, by=list(expgdd_bbd$site,expgdd_bbd$year), FUN=mean,na.rm=TRUE)
soilmois_precip<-aggregate(expclim2$soilmois1, by=list(expclim2$site,expclim2$preciptreat_amt), FUN=mean,na.rm=TRUE)
colnames(soilmois_target)<-c("site","target","soilmois1")
colnames(soilmois_precip)<-c("site","precip_amt","soilmois1")
soilmois_precip<-soilmois_precip[!soilmois_precip$site=="exp06"|soilmois_precip$site=="exp11",]#no moisture data for these 2 sites
soilmois_target<-soilmois_target[!soilmois_target$site=="exp06"|soilmois_target$site=="exp11",]


#Now gdd models
gddmod_bbd<-lmer(cumgdd_air~soilmois_janmar + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_bbd)
summary(gddmod_bbd)
quartz(height=5, width=4.5)
plot(expgdd_bbd$soilmois_janmar,expgdd_bbd$cumgdd_air,type="p",bg=cols[as.numeric(as.factor(expgdd_bbd$site))], pch=21,xlab="Soil moisture (Jan-Mar)", ylab="GDDcrit", bty="l")
abline(a=fixef(gddmod_bbd)[1],b=fixef(gddmod_bbd)[2], lwd=2)



gddmod_lod<-lmer(cumgdd_air~smaj_cent + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_lod)
summary(gddmod_lod)
quartz(height=5, width=4.5)
plot(expgdd_lod$smaj_cent,expgdd_lod$cumgdd_air,type="p",bg=cols[as.numeric(as.factor(expgdd_lod$site))], pch=21,xlab="Soil moisture (Apr-Jun)", ylab="GDDcrit", bty="l")
abline(a=fixef(gddmod_lod)[1],b=fixef(gddmod_lod)[2], lwd=2)

gddmod_lud<-lmer(cumgdd_air~smaj_cent + (1|genus.species)+ (1|site/year), REML=FALSE, data=expgdd_lud)
summary(gddmod_lud)
quartz(height=5, width=4.5)
plot(expgdd_lud$smaj_cent,expgdd_lod$cumgdd_air,type="p",bg=cols[as.numeric(as.factor(expgdd_lod$site))], pch=21,xlab="Soil moisture (Apr-Jun)", ylab="GDDcrit", bty="l")
abline(a=fixef(gddmod_lod)[1],b=fixef(gddmod_lod)[2], lwd=2)

#Make figure of phenology DOY vs. agtemp and soilmois
quartz(height=6,width=10)
par(mfrow=c(1,2))
plot(expall_lud$agtemp_max,expall_lud$doy,type="p", pch=21,bg="gray", ylab="Leaf unfolding DOY", xlab="Above-ground temperature (C)", bty="l")
plot(expall_lud$soilmois1,expall_lud$doy,type="p", pch=21,bg="gray", ylab="Leaf unfolding DOY", xlab="Soil moisture", bty="l")

#Look at relationship between agtemp and soilmoisture in controls versus warming treatments
expclim_noprecip<-expclim2[!expclim2$preciptreat=="-1",]#remove precip treatments
expclim_noprecip<-expclim_noprecip[!expclim_noprecip$preciptreat=="1",]#remove precip treatments

expclim_cont<-expclim_noprecip[which(expclim_noprecip$temptreat=="ambient"|expclim_noprecip$temptreat=="0"),]#select controls
expclim_treat<-expclim_noprecip[-which(expclim_noprecip$temptreat=="ambient"|expclim_noprecip$temptreat=="0"),]#select only trmeatns

#Now fit relationhip between agtemp and soil moisture for controls
sm_agtempmod<-lmer(soilmois1~agtemp_max + (1|site/year/doy), REML=FALSE, data=expclim_cont)
summary(sm_agtempmod)
fixef(sm_agtempmod)
sm_agtempmod_treats<-lmer(soilmois1~agtemp_max + (1|site/year/doy), REML=FALSE, data=expclim_treat)
summary(sm_agtempmod_treats)
fixef(sm_agtempmod_treats)

#Now fit relationhip between agtemp and soil moisture for controls
quartz()
plot(expall_cont$agtemp_max,expall_cont$soilmois1,type="p", pch=21,bg="gray", ylab="Soil moisture", xlab="Above-ground temperature (C)", bty="l")
r=round(cor.test(expall_cont$soilmois1,expall_cont$agtemp_max)$estimate,digits=2)
p=round(cor.test(expall_cont$soilmois1,expall_cont$agtemp_max)$p.value,digits=3)

quartz()
plot(expclim2$agtemp_max,expclim2$soilmois1,type="p", pch=21,bg="lightgray",col="gray", ylab="Soil moisture", xlab="Above-ground temperature (C)", bty="l")
abline(lm(expclim2$soilmois1~expclim2$agtemp_max), lwd=2)
mtext("r=-0.20,p<0.01",side=3, adj=1, line=-1)
cor(expall_subs$soilmois1,expall_subs$agtemp_max)#they're not correlated for bud burst only!
mod<-lm(soilmois1~agtemp_max, data= expall_subs)
summary(mod)

quartz(height=6,width=10)
par(mfrow=c(1,2))
plot(expall_bbd$agtemp_max,expall_bbd$doy,type="p", pch=21,bg=as.numeric(as.factor(expall_bbd$site)), ylab="Budburst DOY", xlab="Above-ground temperature (C)", bty="l")
plot(expall_bbd$soilmois1,expall_bbd$doy,type="p", pch=21,bg=as.numeric(as.factor(expall_bbd$site)), ylab="Budburst DOY", xlab="Soil moisture", bty="l")

quartz(height=6,width=10)
par(mfrow=c(1,2))
plot(expall_lod$agtemp_max,expall_lod$doy,type="p", pch=21,bg="gray", ylab="Leafout DOY", xlab="Above-ground temperature (C)", bty="l")
plot(expall_lod$soilmois1,expall_lod$doy,type="p", pch=21,bg="gray", ylab="Leafout DOY", xlab="Soil moisture", bty="l")

mtext(paste("controls: r=",r,"p=",p),side=3, adj=1, line=-1)

###Now add warmed plots to this figure
expall_treats<-expclim2[-which(expclim2$temptreat=="ambient"|expclim2$temptreat=="0"),]#select controls
expall_treats<-expall_treats[!expall_treats$preciptreat=="-1",]#remove precip treatments
expall_treats<-expall_treats[!expall_treats$preciptreat=="1",]#remove precip treatments
points(expall_treats$agtemp_max,expall_treats$soilmois1,type="p", pch=21,bg="red")
abline(lm(expall_treats$soilmois1~expall_treats$agtemp_max), col="red",lwd=2)
abline(lm(expall_cont$soilmois1~expall_cont$agtemp_max), lwd=2)
r=round(cor.test(expall_treats$soilmois1,expall_treats$agtemp_max)$estimate,digits=2)
p=round(cor.test(expall_treats$soilmois1,expall_treats$agtemp_max)$p.value,digits=3)
mtext(paste("warmed: r=",r,"p=",p),side=3, adj=1, line=-2, col="red")


quartz()
plot(expall_bbd2$soilmois1,expall_bbd2$cumgdd_air,type="p", pch=21,bg="lightgreen", ylab="BudBurst GDDcrit", xlab="Soil moisture", bty="l")


#Old analyses (for 2017 ESA abstract)
#Possible approaches:
#1) Fit models to estimate gddcrit by species, plot and year. then look at GDDcrt values by moisture level?
#2) OR fit ONE model with GDDcrit as the response, species/site and year as random effects, and different combinations of warming/precip treatments check AICs
#Try 2) first, but just with sites that manipulated precip in some way. 
#exppheno4<-exppheno3[which(exppheno3$site=="exp01"|exppheno3$site=="exp02"|exppheno3$site=="exp05"|exppheno3$site=="exp09"|exppheno3$site=="exp12"),]


expall$year<-as.factor(expall$year)
#expall$preciptreat<-as.factor(expall$preciptreat)
#expall$temptreat<-as.factor(expall$temptreat)
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

#for exp02 & exp05, the only controls are ambient controls. code these as 0s to fascilitate comparisons
expall_tp[which(expall_tp$site=="exp02" & expall_tp$temptreat=="ambient"),]$temptreat<-0
expall_tp[which(expall_tp$site=="exp05" & expall_tp$temptreat=="ambient"),]$temptreat<-0


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

expall_tp$temptreat <- relevel(as.factor( expall_tp$temptreat), ref = "0")
expall_tp$preciptreat <- relevel(as.factor( expall_tp$preciptreat), ref = "0")
m1_all1<-lmer(cumgdd_air~temptreat*preciptreat + (temptreat*preciptreat|site/genus.species)+ (1|year), data=expall_tp)
m1_all2<-lmer(cumgdd_air~temptreat*preciptreat + (1|site/genus.species)+ (1|year), data=expall_tp)
m1_all3<-lmer(cumgdd_air~temptreat + (1|site/genus.species)+ (1|year), data=expall_tp)
m1_all4<-lmer(cumgdd_air~temptreat + (temptreat|site/genus.species)+ (1|year), data=expall_tp)
AIC(m1_all1,m1_all2,m1_all3,m1_all4)
aictab(list(m1_all1,m1_all2,m1_all3,m1_all4))
#m1_all2 has lowest aic 
summary(m1_all2)
Anova(m1_all2)

sm_gddmod<-lmer(cumgdd_air~soilmois1 + (soilmois1|site/genus.species)+ (1|year), data=expall_tp)
summary(sm_gddmod)
expall_tp2<-subset(expall_tp,select=c(doy,soilmois1,agtemp_max,agtemp_min,site,year,genus.species))
expall_tp2 <- expall_tp2[apply(expall_tp2, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#scale variables:
expall_tp2$soilmois1_cent<-scale(expall_tp2$soilmois1, center = TRUE, scale = TRUE)
expall_tp2$agtemp_min_cent<-scale(expall_tp2$agtemp_min, center = TRUE, scale = TRUE)
expall_tp2$agtemp_max_cent<-scale(expall_tp2$agtemp_max, center = TRUE, scale = TRUE)

sm_doymod5<-lmer(doy~agtemp_min_cent*soilmois1_cent + (1|site/genus.species)+ (1|year), REML=TRUE, data=expall_tp2)
sm_doymod6<-lmer(doy~agtemp_min_cent*soilmois1_cent + (agtemp_min_cent*soilmois1_cent|site/genus.species)+ (1|year), REML=TRUE, data=expall_tp2)
AIC(sm_doymod5,sm_doymod6)#to get random effects, 6 wins

sm_doymod6<-lmer(doy~agtemp_min_cent*soilmois1_cent + (agtemp_min_cent*soilmois1_cent|site/genus.species)+ (1|year), REML=FALSE, data=expall_tp2)
sm_doymod1<-lmer(doy~soilmois1_cent + (soilmois1_cent|site/genus.species)+ (1|year), data=expall_tp2)
sm_doymod2<-lmer(doy~soilmois1+agtemp_max_cent + (soilmois1_cent+agtemp_max_cent|site/genus.species)+ (1|year), REML=FALSE,data=expall_tp2)
sm_doymod3<-lmer(doy~soilmois1*agtemp_max_cent + (soilmois1_cent*agtemp_max_cent|site/genus.species)+ (1|year), REML=FALSE,data=expall_tp2)
sm_doymod4<-lmer(doy~soilmois1+agtemp_min_cent + (soilmois1_cent+agtemp_min_cent|site/genus.species)+ (1|year), REML=FALSE,data=expall_tp2)
sm_doymod1a<-lmer(doy~agtemp_max_cent + (agtemp_max_cent|site/genus.species)+ (1|year), data=expall_tp2)
sm_doymod1b<-lmer(doy~agtemp_min_cent + (agtemp_min_cent|site/genus.species)+ (1|year), data=expall_tp2)

AIC(sm_doymod1,sm_doymod1a,sm_doymod1b,sm_doymod2,sm_doymod3,sm_doymod4,sm_doymod6)
#sm_doymod6 wins
summary(sm_doymod1a)
Anova(sm_doymod6, type="III")
coef(sm_doymod6)
#Surprising: sil moisture explains more variation than temperature
cor.test(expall_tp2$agtemp_min_cent, expall_tp2$soilmois1_cent)


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



###Analyses for Stan Meeting
##Model-fitting for soil moisture and phenology analyses
##Started Apsil 24, 2017
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
#install.packages('devtools')
#devtools::install_github('rmcelreath/glmer2stan')
#options(mc.cores = parallel::detectCores())

## load packages
library("rstan") # observe startup messages
library(lme4)
library(glmer2stan)
library(devtools)

##Two main questions to address:
#1) How do experimental warming and precipitation treatments affect soil moisture?
#2) How does soil moisture affect GDDcrit?

##We will try to fit models to address these questions in lmer and in stan
#1)How do experimental warming and precipitation treatments affect soil moisture?
#read in experimental climate data
setwd("~/git/gelmanhill/expphen")
expclim<-read.csv("expclim2.csv", header=T)
head(expclim)
expclim2<-subset(expclim,select=c(site,year,doy,target_cent,preciptreat_amt_cent,soilmois1))
expclim2  <- expclim2 [apply(expclim2 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na

#fit model in lmer
sm_lme4<-lmer(soilmois1~target_cent*preciptreat_amt_cent + (1|site)+ (1|year/doy), REML=FALSE, data=expclim2)
summary(sm_lme4)

# construct subject index --- glmer2stan forces you to manage your own cluster indices
expclim2$site_index = as.integer(as.factor(expclim2$site))
expclim2$year_index = as.integer(as.factor(expclim2$year))
expclim2$doy_index = as.integer(as.factor(expclim2$doy))

# fit with lmer2stan- this did not work! took a really long time....
sm_stan = lmer2stan(soilmois1~target_cent + (1|site_index), data=expclim2)
sm_stan

#2) How does soil moisture affect GDDcrit?
expphen<-read.csv("exphengdd.csv", header=T)
head(expphen)
expphen2<-subset(expphen,select=c(site,year,doy,warm_treat,precip_treat,sm,cumgdd_air,genus.species))
expphen2  <- expphen2 [apply(expphen2 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
gdd_lme4A<-lmer(cumgdd_air~sm + (1|site)+ (1|genus.species), REML=FALSE, data=expphen2)
summary(gdd_lme4A)
#OR
gdd_lme4B<-lmer(cumgdd_air~sm + (sm|site)+ (sm|genus.species), REML=FALSE, data=expphen2)
summary(gdd_lme4B)

#OR
gdd_lme4C<-lmer(cumgdd_air~sm + (sm|site/genus.species), REML=FALSE, data=expphen2)
summary(gdd_lme4C)
###Notes from meeting and stuff to try next:
###For soil moisture model, treatment model:
#1) DOY should be nested within site
#2) may want to model temporal trend in soil moisture
#3) Use study year, or center calendar year.
#For GDDcrit model:
#1) Add random effect of year (nested within site?)
#2) Think about soil moisture: using mean across whole year, but should you use mean up to point of the event?
#3) Center everything- year (or use study year), soil moisutre, GDD crit
#4) check how much overlap in species there is across sites.


## Fit preliminary models to get estimate of growing degree days at phenological events
## in each plot/species/site
##Compare GDDcrit in different treatments
exppheno$genus.species<-paste(exppheno$genus,exppheno$species,sep=".")
exppheno[which(is.na(exppheno$block)),]$block<-"none"
expclim[which(is.na(expclim$block)),]$block<-"none"

#merge phenology data with experimental climate to get gdd crit
exp_gdd<-inner_join(exppheno,expclim, by=c("site", "block", "plot","year","doy"), match="all")
#select out just columns wanted to fit model with soil moisture and aboveground temp
exp_gdd2<-subset(exp_gdd,selec=c(site,block,plot,event,year,genus,species,genus.species,doy,temptreat,preciptreat,cumgdd_soil,numnas_soilgdd,cumgdd_air,numnas_airgdd))
#merge in target temp, precip treatment, and annual ag temp and soil moisture values
exp_gdd3<-left_join(exp_gdd2,tempsm_plots, by=c("site", "block", "plot","year"))
#replace controls (=NAs) with 0 for warming treatming and precip treatment
exp_gdd3[which(is.na(exp_gdd3$warm_treat)),]$warm_treat<-0
exp_gdd3[which(is.na(exp_gdd3$precip_treat)),]$precip_treat<-100
exp_gdd3<-exp_gdd3[order(exp_gdd3$site,exp_gdd3$block,exp_gdd3$plot,exp_gdd3$year),]
tempsm_plots<-tempsm_plots[order(tempsm_plots$site,tempsm_plots$block,tempsm_plots$plot,tempsm_plots$year),]
###Make boxplot of gddcrit by 
unique(exp_gdd3$genus.species)#241 species

##now aggregate monthly too:
expclim2$date<-strptime(strptime(paste(expclim2$year,expclim2$doy,sep="-"), format = "%Y-%j"),format = "%Y-%m-%d")
expclim2$month<-substr(expclim2$date,6,7)
ag_max_month<-aggregate(expclim2$agtemp_max, by=list(expclim2$site,expclim2$block,expclim2$plot,expclim2$target,expclim2$preciptreat_amt,expclim2$year,expclim2$month), FUN=mean,na.rm=TRUE)
ag_min_month<-aggregate(expclim2$agtemp_min, by=list(expclim2$site,expclim2$block,expclim2$plot,expclim2$target,expclim2$preciptreat_amt,expclim2$year,expclim2$month), FUN=mean,na.rm=TRUE)
soilmois_month<-aggregate(expclim2$soilmois1, by=list(expclim2$site,expclim2$block,expclim2$plot,expclim2$target,expclim2$preciptreat_amt,expclim2$year,expclim2$month), FUN=mean,na.rm=TRUE)
#combine into one dataframe
tempsm_month<-cbind(ag_max_month,ag_min_month$x,soilmois_month$x)
colnames(tempsm_month)<-c("site","block","plot","warm_treat","precip_treat","year","month","agtmax","agtmin","sm")
tempsm_month<-tempsm_month[order(tempsm_month$site,tempsm_month$block,tempsm_month$plot,tempsm_month$year,tempsm_month$month),]

#Start by plotting treatment effects on doy and GDD crit
exp_gdd3$all_treat<-paste(exp_gdd3$warm_treat,exp_gdd3$precip_treat, sep=".")
sites<-unique(exp_gdd3$site)
for(i in 1:length(sites)){
  sitedat<-exp_gdd3[exp_gdd3$site==sites[i],]
  quartz()
  par(mfrow=c(1,2))
  boxplot(sitedat$doy~sitedat$all_treat, main=paste(sites[i],"doy"))
  if (length(unique(sitedat$cumgdd_air))>1) {
    boxplot(sitedat$cumgdd_air~sitedat$all_treat, main=paste(sites[i],"gddcrit-air"))
  } else if(length(unique(sitedat$cumgdd_soil))>1){
    boxplot(sitedat$cumgdd_soil~sitedat$all_treat, main=paste(sites[i],"gddcrit-soil"))
  }else (next)
}

#Now plottreatment effects on soil moisture

for(i in 1:length(sites)){
  sitedat<-exp_gdd3[exp_gdd3$site==sites[i],]
  quartz(height=5, width=8)
  par(mfrow=c(1,2))
  if (length(unique(sitedat$sm))>1) {
    boxplot(sitedat$sm~sitedat$all_treat, main=paste(sites[i],"soilmois"), ylab="sm")
  }
  if (length(unique(sitedat$cumgdd_air))>1) {
    plot(sitedat$sm, sitedat$cumgdd_air,type="p",pch=21,main=paste(sites[i],"gddcrit-air"), ylab="gddcrit")
  } else if(length(unique(sitedat$cumgdd_soil))>1){
    plot(sitedat$sm,sitedat$cumgdd_soil, type="p",pch=21,main=paste(sites[i],"gddcrit-soil"), ylab="gddcrit")
  }else (next)
}


