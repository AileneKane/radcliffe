#Preliminary analyses of experimental phenology, soil moisture, and temperature data for radcliffe
#Started March 18, 2016 by Ailene Ettinger
#Modified/added to by Ailene February-April 2017 for ESA abstract and additional analyses
###Three questions to address:
#1) How do warming and precip treatments affect soil moisture? (Make plots and fit models)
#2) How does soil moisture affect GDDcrit?
#3) How do soil moisture and temperature affect doy of bud burst, leaf out, etc?

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
## load packages
library(RColorBrewer)
library(lme4)
#library(car)
library(dplyr)
library(visreg)
#Read in experimental climate and phenology data
setwd("~/git/radcliffe")
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)

#standard data wrangling to get expclim2 for climate analyses and expgdd for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")

#summarize climate data by plot (annual and seasonal temp, soil mois), 
#merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
source("Analyses/soilmoisture/climsum_byplot.R")

#Make a figure that matches the schematic figure in Exp CC, just for budburst
#1) How does warming affect phenology (traditional approach, assuming only direct effects)

#select out only bbd
expgdd_bbd<-expgdd_subs[which(expgdd_subs$event=="bbd"),]#
expgdd_bbd$styear<-as.factor(expgdd_bbd$styear)
#model with actual temp/soil mois data
smbbdmod<-lmer(doy~ag_max_janmar*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_bbd)
summary(smbbdmod)
#subtract minimum agtmax to get it close to 0 warming to make scale more comparable to "target wamring"
expgdd_bbd$agtmax_rel<-expgdd_bbd$agtmax-min(expgdd_bbd$agtmax)
expgdd_bbd$agtmin_rel<-expgdd_bbd$agtmin-min(expgdd_bbd$agtmin)

smbbdmod1<-lmer(doy~agtmax_rel*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_bbd)
summary(smbbdmod1)
smbbdmod1a<-lmer(doy~agtmin_rel*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_bbd)
summary(smbbdmod1a)
smbbdmod2<-lmer(doy~target*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_bbd)
summary(smbbdmod2)

smbbdmod_targt<-lmer(doy~target + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_bbd)
summary(smbbdmod_targt)

smbbdmod_agtmax<-lmer(doy~ag_max_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_bbd)
summary(smbbdmod_agtmax)
#try to figure out why intercept changes so much in the measured variables versus target temp
min(expgdd_bbd$target); max(expgdd_bbd$target) #range is from 0 to 5.5
min(expgdd_bbd$agtmax); max(expgdd_bbd$agtmax) #range is from 12.43226 to 28.30171
#Aha! the intercept changes so much because the range of the explanatory variable switches from being amount of warming to above0ground temperature.
#Should we do anything about this? or just explain it!
#tried make relative column above. look at range:
min(expgdd_bbd$agtmin_rel); max(expgdd_bbd$agtmin_rel)
#plot with fitted lines only (no points) and with soil moisture as well
quartz(height=5, width=10)
par(mfrow=c(1,2), oma=c(.5,.5,.5,2))
plot(1,type="n",xlab="Treatment intensity", ylab="Day of year", bty="l",xlim=c(min(expgdd_bbd$target),max(expgdd_bbd$target)),ylim=c(60,200), las=1, main="Budburst")
#for(i in 1:dim(ranef(smbbdmod2)$site)[1]){
#  abline(a=coef(smbbdmod2)$site[i,1],b=fixef(smbbdmod2)[2], lwd=1, lty=2,col=cols2[i])#temp ranef
#}
abline(a=fixef(smbbdmod1a)[1],b=fixef(smbbdmod1a)[2], lwd=3, col="darkred", lty=2)#actual ag temp coef
abline(a=fixef(smbbdmod_targt)[1],b=fixef(smbbdmod_targt)[2], lwd=3)
mtext("Increasing above-ground temperature",side=1, line=4, cex=.9)
mtext("Decreasing soil moisture",side=1, line=4.5, cex=.9)

#now add soil moisture  
par(new=TRUE)

plot(1, xlab="", xlim=c(0.38,.11),ylim=c(60,200), axes=FALSE, type="b")
abline(a=fixef(smbbdmod1a)[1],b=fixef(smbbdmod1a)[3], lwd=3, col="blue", lty=3)#soil moisture coef 
legend("topleft",legend=c("Effect of temperature", "Effect of soil moisture","Observed response to target warming"), lty=c(2,3,1), lwd=2, col=c("darkred","blue","black"), bty="n", cex=.8)



####################
####Leafout Data####
####################
expgdd_lod<-expgdd_subs[which(expgdd_subs$event=="lod"),]#
expgdd_lod$styear<-as.factor(expgdd_lod$styear)
#model with actual temp/soil mois data
smlodmod<-lmer(doy~ag_max_janmar*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_lod)
smlodmoda<-lmer(doy~ag_max_aprjun*soilmois_aprjun + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_lod)

summary(smlodmod)
#subtract minimum agtmax to get it close to 0 warming to make scale more comparable to "target wamring"
expgdd_lod$agtmax_rel<-expgdd_lod$agtmax-min(expgdd_lod$agtmax)
expgdd_lod$agtmin_rel<-expgdd_lod$agtmin-min(expgdd_lod$agtmin)

smlodmod1<-lmer(doy~agtmax_rel*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_lod)
smlodmod1aa<-lmer(doy~agtmax_rel*soilmois_aprjun + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_lod)

summary(smlodmod1)
smlodmod1a<-lmer(doy~agtmin_rel*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_lod)
summary(smbbdmod1a)
smlodmod2<-lmer(doy~target*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_lod)
smlodmod2a<-lmer(doy~target*soilmois_aprjun + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_lod)

summary(smlodmod2)

smlodmod_targt<-lmer(doy~target + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_lod)
summary(smlodmod_targt)

smlodmod_agtmax<-lmer(doy~ag_max_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_lod)
smlodmod_agtmaxa<-lmer(doy~ag_max_aprjun + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_lod)

summary(smlodmod_agtmax)
AIC(smlodmod,smlodmod_targt,smlodmod_agtmax,smlodmod2,smlodmod1,smlodmod1a,smlodmoda,smlodmod1aa,smlodmod2a)
#smlodmod wins: leaf out affected by amtempmax, soilmois_janmar, and interaction
#quartz(height=5, width=10)
plot(1,type="n",xlab="Treatment intensity", ylab="Day of year", bty="l",xlim=c(min(expgdd_lod$target),max(expgdd_lod$target)),ylim=c(60,200), las=1, main="Leafout")
#for(i in 1:dim(ranef(smbbdmod2)$site)[1]){
#  abline(a=coef(smbbdmod2)$site[i,1],b=fixef(smbbdmod2)[2], lwd=1, lty=2,col=cols2[i])#temp ranef
#}
abline(a=fixef(smlodmod1)[1],b=fixef(smlodmod1)[2], lwd=3, col="darkred", lty=2)#actual ag temp coef
abline(a=fixef(smlodmod_targt)[1],b=fixef(smlodmod_targt)[2], lwd=3)

#now add soil moisture  
par(new=TRUE)

plot(1, xlab="", xlim=c(0.38,.11),ylim=c(60,200), axes=FALSE, type="b")
abline(a=fixef(smlodmod1)[1],b=fixef(smlodmod1)[3], lwd=3, col="blue", lty=3)#soil moisture coef 



###Plots with lines
cols <- brewer.pal(6,"Spectral")

quartz(height=5, width=9)
par(mfrow=c(1,2))

plot(1,type="n",xlab="Mean soil moisture (Jan-Mar)", ylab="Day of year", bty="l",xlim=c(0,max(expgdd_bbd$sm)),ylim=c(20,200), main="Budburst")
for(i in 1:dim(ranef(smbbdmod1a)$site)[1]){
  abline(a=coef(smbbdmod1a)$site[i,1],b=fixef(smbbdmod1a)[3], lwd=1, col=cols[i])
}
abline(a=fixef(smbbdmod1a)[1],b=fixef(smbbdmod1a)[3], lwd=2)
legend(.24, 60,legend=unique(expgdd_bbd$site),lty=1, col=cols[as.numeric(as.factor(unique(expgdd_bbd$site)))], cex=0.6)
plot(1,type="n",xlab="Mean soil moisture (Jan-Mar)", ylab="Day of year", bty="l",xlim=c(0,max(expgdd_bbd$sm)),ylim=c(20,200), main="Leafout")
for(i in 1:dim(ranef(smlodmod)$site)[1]){
  abline(a=coef(smlodmod)$site[i,1],b=fixef(smlodmod)[3], lwd=1, col=cols[i])
}
abline(a=fixef(smlodmod)[1],b=fixef(smlodmod)[3], lwd=2)

####################
####Flowering Data####
####################
expgdd_ffd<-expgdd_subs[which(expgdd_subs$event=="ffd"),]#
expgdd_ffd$styear<-as.factor(expgdd_ffd$styear)
#model with actual temp/soil mois data
smffdmod<-lmer(doy~ag_max_janmar*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd)
smffdmoda<-lmer(doy~ag_max_aprjun*soilmois_aprjun + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd)
summary(smffdmod)
#subtract minimum agtmax to get it close to 0 warming to make scale more comparable to "target wamring"
expgdd_ffd$agtmax_rel<-expgdd_ffd$agtmax-min(expgdd_ffd$agtmax)
expgdd_ffd$agtmin_rel<-expgdd_ffd$agtmin-min(expgdd_ffd$agtmin)

smffdmod1<-lmer(doy~agtmax_rel*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd)
smffdmod1aa<-lmer(doy~agtmax_rel*soilmois_aprjun + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd)

summary(smffdmod1)
smffdmod1a<-lmer(doy~agtmin_rel*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd)
summary(smbbdmod1a)
smffdmod2<-lmer(doy~target*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd)
smffdmod2a<-lmer(doy~target*soilmois_aprjun + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd)

summary(smffdmod2)

smffdmod_targt<-lmer(doy~target + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd)
summary(smffdmod_targt)

smffdmod_agtmax<-lmer(doy~ag_max_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd)
smffdmod_agtmaxa<-lmer(doy~ag_max_aprjun + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd)

summary(smffdmod_agtmax)
AIC(smffdmod,smffdmod_targt,smffdmod_agtmax,smffdmod2,smffdmod1,smffdmod1a,smffdmoda,smffdmod1aa,smffdmod2a)


#Flowering
quartz(height=5, width=9)
par(mfrow=c(1,2))
#temp
plot(expgdd_ffd$ag_max_janmar[expgdd_ffd$site=="exp01"],expgdd_ffd$doy[expgdd_ffd$site=="exp01"],type="p",pch=21,bg="#D53E4F",xlab="Mean air temp (Jan-Mar)", ylab="Day of year", bty="l",xlim=c(min(expgdd_ffd$ag_max_janmar),max(expgdd_ffd$ag_max_janmar)),ylim=c(20,350))
points(expgdd_ffd$ag_max_janmar[expgdd_ffd$site=="exp10"],expgdd_ffd$doy[expgdd_ffd$site=="exp10"],type="p",pch=21,bg="#99D594")
points(expgdd_ffd$ag_max_janmar[expgdd_ffd$site=="exp12"],expgdd_ffd$doy[expgdd_ffd$site=="exp12"],type="p",pch=21,bg="#3288BD")
cols<-c("#D53E4F","#99D594","#3288BD")
for(i in 1:dim(ranef(smffdmod)$site)[1]){
  abline(a=coef(smffdmod)$site[i,1],b=fixef(smffdmod)[2], lwd=1, col=cols[i])
}
abline(a=fixef(smffdmod)[1],b=fixef(smffdmod)[2], lwd=2)
#soil moisture
plot(expgdd_ffd$soilmois_janmar[expgdd_ffd$site=="exp01"],expgdd_ffd$doy[expgdd_ffd$site=="exp01"],type="p",pch=21,bg="#D53E4F",xlab="Mean soil moisture (Jan-Mar)", ylab="Day of year", bty="l",xlim=c(.2,max(expgdd_ffd$sm)),ylim=c(20,350))
points(expgdd_ffd$soilmois_janmar[expgdd_ffd$site=="exp10"],expgdd_ffd$doy[expgdd_ffd$site=="exp10"],type="p",pch=21,bg="#99D594")
points(expgdd_ffd$soilmois_janmar[expgdd_ffd$site=="exp12"],expgdd_ffd$doy[expgdd_ffd$site=="exp12"],type="p",pch=21,bg="#3288BD")
cols<-c("#D53E4F","#99D594","#3288BD")
for(i in 1:dim(ranef(smffdmod)$site)[1]){
  abline(a=coef(smffdmod)$site[i,1],b=fixef(smffdmod)[3], lwd=1, col=cols[i])
}
abline(a=fixef(smffdmod)[1],b=fixef(smffdmod)[3], lwd=2)

legend(.28, 70,legend=unique(expgdd_ffd$site),lty=1, pch=21, pt.bg=cols[as.numeric(as.factor(unique(expgdd_ffd$site)))], cex=0.6)

#effects seem to differ for early vs late flowering. divide at day 182 (july 1)
expgdd_ffd_late<-expgdd_ffd[expgdd_ffd$doy>=182,]#
expgdd_ffd_early<-expgdd_ffd[expgdd_ffd$doy<182,]#

#plot historgram of flowering times
quartz(height=3, width=9)
par(mfrow=c(1,3))
hist(expgdd_ffd[expgdd_ffd$site=="exp01",]$doy, xlab="doy", main="exp01")
hist(expgdd_ffd[expgdd_ffd$site=="exp10",]$doy, xlab="doy", main="exp10")
hist(expgdd_ffd[expgdd_ffd$site=="exp12",]$doy, xlab="doy", main="exp12")

##Early flowering
expgdd_ffd_early$styear<-as.factor(expgdd_ffd_early$styear)
#model with actual temp/soil mois data
smffd_earlymod<-lmer(doy~ag_max_janmar*soilmois_janmar + (1|genus.species)+ (1|site), REML=FALSE, data=expgdd_ffd_early)
smffd_earlymoda<-lmer(doy~ag_max_aprjun*soilmois_aprjun + (1|genus.species)+ (1|site), REML=FALSE, data=expgdd_ffd_early)
summary(smffd_earlymod)
#subtract minimum agtmax to get it close to 0 warming to make scale more comparable to "target wamring"
expgdd_ffd_early$agtmax_rel<-expgdd_ffd_early$agtmax-min(expgdd_ffd_early$agtmax)
expgdd_ffd_early$agtmin_rel<-expgdd_ffd_early$agtmin-min(expgdd_ffd_early$agtmin)

smffd_earlymod1<-lmer(doy~agtmax_rel*soilmois_janmar + (1|genus.species)+ (1|site), REML=FALSE, data=expgdd_ffd_early)
smffd_earlymod1aa<-lmer(doy~agtmax_rel*soilmois_aprjun + (1|genus.species)+ (1|site), REML=FALSE, data=expgdd_ffd_early)

summary(smffd_earlymod1)
smffd_earlymod1a<-lmer(doy~agtmin_rel*soilmois_janmar + (1|genus.species)+ (1|site), REML=FALSE, data=expgdd_ffd_early)
summary(smbbdmod1a)
smffd_earlymod2<-lmer(doy~target*soilmois_janmar + (1|genus.species)+ (1|site), REML=FALSE, data=expgdd_ffd_early)
smffd_earlymod2a<-lmer(doy~target*soilmois_aprjun + (1|genus.species)+ (1|site), REML=FALSE, data=expgdd_ffd_early)

summary(smffd_earlymod2)

smffd_earlymod_targt<-lmer(doy~target + (1|genus.species)+ (1|site), REML=FALSE, data=expgdd_ffd_early)
summary(smffd_earlymod_targt)

smffd_earlymod_agtmax<-lmer(doy~ag_max_janmar + (1|genus.species)+ (1|site), REML=FALSE, data=expgdd_ffd_early)
smffd_earlymod_agtmaxa<-lmer(doy~ag_max_aprjun + (1|genus.species)+ (1|site), REML=FALSE, data=expgdd_ffd_early)

summary(smffd_earlymod_agtmax)
aictable<-AIC(smffd_earlymod,smffd_earlymod_targt,smffd_earlymod_agtmax,smffd_earlymod2,smffd_earlymod1,smffd_earlymod1a,smffd_earlymoda,smffd_earlymod1aa,smffd_earlymod2a)
aictable[order(aictable$AIC),]

#latete flowering
expgdd_ffd_late$styear<-as.factor(expgdd_ffd_late$styear)
#model with actual temp/soil mois data
smffd_latemod<-lmer(doy~ag_max_janmar*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd_late)
smffd_latemoda<-lmer(doy~ag_max_aprjun*soilmois_aprjun + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd_late)
summary(smffd_latemod)
#subtract minimum agtmax to get it close to 0 warming to make scale more comparable to "target wamring"
expgdd_ffd_late$agtmax_rel<-expgdd_ffd_late$agtmax-min(expgdd_ffd_late$agtmax)
expgdd_ffd_late$agtmin_rel<-expgdd_ffd_late$agtmin-min(expgdd_ffd_late$agtmin)

smffd_latemod1<-lmer(doy~agtmax_rel*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd_late)
smffd_latemod1aa<-lmer(doy~agtmax_rel*soilmois_aprjun + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd_late)

summary(smffd_latemod1)
smffd_latemod1a<-lmer(doy~agtmin_rel*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd_late)
summary(smbbdmod1a)
smffd_latemod2<-lmer(doy~target*soilmois_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd_late)
smffd_latemod2a<-lmer(doy~target*soilmois_aprjun + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd_late)

summary(smffd_latemod2)

smffd_latemod_targt<-lmer(doy~target + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd_late)
summary(smffd_latemod_targt)

smffd_latemod_agtmax<-lmer(doy~ag_max_janmar + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd_late)
smffd_latemod_agtmaxa<-lmer(doy~ag_max_aprjun + (1|genus.species)+ (1|site/styear), REML=FALSE, data=expgdd_ffd_late)

summary(smffd_latemod_agtmax)
atab<-AIC(smffd_latemod,smffd_latemod_targt,smffd_latemod_agtmax,smffd_latemod2,smffd_latemod1,smffd_latemod1a,smffd_latemoda,smffd_latemod1aa,smffd_latemod2a)

atab[order(atab$AIC),]


###Try some analyses with different experimental treatments
colnames(expgdd)
table(expgdd$site,expgdd$preciptreat_amt)
expp<-expgdd[expgdd$site=="exp01"|expgdd$site=="exp02"|expgdd$site=="exp05"|expgdd$site=="exp09"|expgdd$site=="exp12",]#these are the sites that manipulate precip
expp_bbd<-expp[expp$event=="bbd",]
expp_bbd<-subset(expp_bbd,select=c(site,plot,year,target,preciptreat_amt,event,doy,genus.species))
expp_bbd <- expp_bbd[apply(expp_bbd, 1, function(x) all(!is.na(x))),]
# only keep rows of all not na

mod<-lmer(doy~target*preciptreat_amt+ (1|genus.species)+(1|site/year), REML=FALSE, data=expp)
bbdmod<-lmer(doy~target*preciptreat_amt+ (1|genus.species), REML=FALSE, data=expp_bbd)
summary(bbdmod)
