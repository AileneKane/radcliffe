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

#Make plots of these two models
cols <- brewer.pal(6,"Greys")
#With points showing mean BBDOY for each plot/year/site
#quartz(height=5, width=8)
#par(mfrow=c(1,2), omi=c(.4,.4,.2,.5))
#mean bbdoy by plot, year, and site
#bbdoy_plot<-aggregate(expgdd_bbd$doy, by=list(expgdd_bbd$site,expgdd_bbd$block,expgdd_bbd$plot,expgdd_bbd$target,expgdd_bbd$preciptreat_amt,expgdd_bbd$year), FUN=mean,na.rm=TRUE)
#colnames(bbdoy_plot)<-c("site","block","plot","target","preciptreat_amt","year","bbdoy")
#plot(bbdoy_plot$target,bbdoy_plot$bbdoy,type="p",bg=cols[as.numeric(as.factor(bbdoy_plot$site))], pch=21,xlab="Target warming", ylab="Day of year", bty="l", main="Target Temp Mod", xlim=c(0,6))
#abline(a=fixef(smbbdmod_targt)[1],b=fixef(smbbdmod_targt)[2], lwd=2)

#plot(bbdoy_plot$target,bbdoy_plot$bbdoy,type="p",bg=cols[as.numeric(as.factor(bbdoy_plot$site))], pch=21,xlab="Target warming", ylab="Day of year", bty="l", main="Measured Temp Mod")
#abline(a=fixef(smbbdmod_targt)[1],b=fixef(smbbdmod_targt)[2], lwd=2)
#abline(a=fixef(smbbdmod_agtmax)[1],b=fixef(smbbdmod_agtmax)[2], lty=2,lwd=2)#target temp only mod

#plot(bbdoy_plot$target,bbdoy_plot$bbdoy,type="p",bg=cols[as.numeric(as.factor(bbdoy_plot$site))], pch=21,xlab="Target warming", ylab="Day of year", bty="l", main="Target Temp & Soil moisture Mod", xlim=c(0,6))
#abline(a=fixef(smbbdmod_targt)[1],b=fixef(smbbdmod_targt)[2], lwd=2)
#abline(a=fixef(smbbdmod2)[1],b=fixef(smbbdmod2)[2], lty=2,lwd=2)#measured temp mod
#legend(5.2, 150,legend=unique(bbdoy_plot$site),pch=21, pt.bg=cols[as.numeric(as.factor(unique(bbdoy_plot$site)))], cex=0.6)


#plot with fitted lines only (no points) and with soil moisture as well

quartz(height=5, width=5)
#par(mfrow=c(1,2), oma=c(.5,.5,.5,2))
#no need to have the target warming plotted on its own- just plot it on the same figure as the measured climate data
#plot(1,type="n",xlab="(Target warming, C)", ylab="Day of year", bty="l", xlim=c(min(bbdoy_plot$target),max(bbdoy_plot$target)),ylim=c(80,180),main="Target warming model",las=1)

#mtext("Treatment intensity",side=1, line=2, cex=1.1)
plot(1,type="n",xlab="Treatment intensity", ylab="Day of year", bty="l",xlim=c(min(expgdd_bbd$target),max(expgdd_bbd$target)),ylim=c(60,160), las=1)
#for(i in 1:dim(ranef(smbbdmod2)$site)[1]){
#  abline(a=coef(smbbdmod2)$site[i,1],b=fixef(smbbdmod2)[2], lwd=1, lty=2,col=cols2[i])#temp ranef
#}
abline(a=fixef(smbbdmod1a)[1],b=fixef(smbbdmod1a)[2], lwd=3, col="darkred", lty=2)#actual ag temp coef
abline(a=fixef(smbbdmod_targt)[1],b=fixef(smbbdmod_targt)[2], lwd=3)
mtext("Increasing above-ground temperature",side=1, line=4, cex=.9)
mtext("Decreasing soil moisture",side=1, line=4.5, cex=.9)

#now add soil moisture  
par(new=TRUE)

plot(1, xlab="", xlim=c(0.5,0),ylim=c(60,160), axes=FALSE, type="b")
abline(a=fixef(smbbdmod1a)[1],b=fixef(smbbdmod1a)[2], lwd=3, col="blue", lty=3)#soil moisture coef 
legend("bottomleft",legend=c("Effect of temperature", "Effect of soil moisture","Observed response to target warming"), lty=c(2,3,1), lwd=2, col=c("darkred","blue","black"), bty="n", cex=.8)


###Same figure but using different axis for true temp
quartz(height=5, width=5)
#par(mfrow=c(1,2), oma=c(.5,.5,.5,2))
#no need to have the target warming plotted on its own- just plot it on the same figure as the measured climate data
#plot(1,type="n",xlab="(Target warming, C)", ylab="Day of year", bty="l", xlim=c(min(bbdoy_plot$target),max(bbdoy_plot$target)),ylim=c(80,180),main="Target warming model",las=1)

#mtext("Treatment intensity",side=1, line=2, cex=1.1)
plot(1,type="n",xlab="Treatment intensity", ylab="Day of year", bty="l",xlim=c(min(bbdoy_plot$target),max(bbdoy_plot$target)),ylim=c(60,160), las=1)
#for(i in 1:dim(ranef(smbbdmod2)$site)[1]){
#  abline(a=coef(smbbdmod2)$site[i,1],b=fixef(smbbdmod2)[2], lwd=1, lty=2,col=cols2[i])#temp ranef
#}
abline(a=fixef(smbbdmod_targt)[1],b=fixef(smbbdmod_targt)[2], lwd=3)
mtext("Increasing above-ground temperature",side=1, line=4, cex=.9)
mtext("Decreasing soil moisture",side=1, line=4.5, cex=.9)
#now add measured temp effect 
par(new=TRUE)

plot(1, xlab="", xlim=c(min(expgdd_bbd$agtmax_rel),max(expgdd_bbd$agtmax_rel)),ylim=c(60,160), axes=FALSE, type="b")
abline(a=fixef(smbbdmod1a)[1],b=fixef(smbbdmod1a)[2], lwd=3, col="darkred", lty=2)#actual ag temp coef

#now add soil moisture  
par(new=TRUE)

plot(1, xlab="", xlim=c(0.5,0),ylim=c(60,160), axes=FALSE, type="b")
abline(a=fixef(smbbdmod1a)[1],b=fixef(smbbdmod1a)[2], lwd=3, col="blue", lty=3)#soil moisture coef 
legend("bottomleft",legend=c("Effect of temperature", "Effect of soil moisture","Observed response to target warming"), lty=c(2,3,1), lwd=2, col=c("darkred","blue","black"), bty="n", cex=.8)






###Not usingfor now:
###Add relationship between sm~target warming figure. 
plot(1,type="n",xlab="(Target warming, C)", ylab="Soil moisture (VWC)", bty="l",xlim=c(min(bbdoy_plot$target),max(bbdoy_plot$target)),ylim=c(0,.5), las=1)
expclim_sm<-subset(expclim2,select=c(site,year,doy,target,preciptreat,soilmois1))
expclim_sm<-expclim_sm[which(expclim_sm$site=="exp01"|expclim_sm$site=="exp02"|expclim_sm$site=="exp03"|expclim_sm$site=="exp04"|expclim_sm$site=="exp07"|expclim_sm$site=="exp09"|expclim_sm$site=="exp10"|expclim_sm$site=="exp12"),]#
expclim_sm<-expclim_sm[!expclim_sm$preciptreat=="-1",]#remove precip treatments
expclim_sm<-expclim_sm[!expclim_sm$preciptreat=="1",]#remove precip treatments
expclim_sm <- expclim_sm[apply(expclim_sm, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
smmod<-lmer(soilmois1~target + (1|site/year/doy), REML=FALSE, data=expclim_sm)
summary(smmod)
abline(a=fixef(smmod)[1],b=fixef(smmod)[2], lwd=3, col="blue", lty=1)#
mtext("Treatment intensity",side=1, line=2, cex=1.1)
cols <- brewer.pal(4,"Blues")

for(i in 1:dim(ranef(smmod)$site)[1]){
  abline(a=coef(smmod)$site[i,1],b=fixef(smmod)[2], lwd=1, col=cols[i])
}
mtext("b)",side=3, line=1, adj=-.25,cex=.9)

#check the aic and predicted vs observed for the different models (smbbdmod1 has lowest AIC, highest R2 for pred vs observed)
quartz(height=5, width=10)
par(mfrow=c(1,4), oma=c(.5,.5,.5,2))
plot(expgdd_bbd$doy,predict(smbbdmod),main="Jan-mar temp mod")
abline(lm(predict(smbbdmod)~expgdd_bbd$doy), col="red")
abline(a=0, b=1, lwd=2)

summary(lm(predict(smbbdmod)~expgdd_bbd$doy))

plot(expgdd_bbd$doy,predict(smbbdmod1),main="Mean ann. max temp mod")
abline(lm(predict(smbbdmod1)~expgdd_bbd$doy), col="red")
abline(a=0, b=1, lwd=2)
summary(lm(predict(smbbdmod1)~expgdd_bbd$doy))

plot(expgdd_bbd$doy,predict(smbbdmod1a),main="Mean ann. min temp mod")
abline(lm(predict(smbbdmod1a)~expgdd_bbd$doy), col="red")
abline(a=0, b=1, lwd=2)
summary(lm(predict(smbbdmod1a)~expgdd_bbd$doy))

plot(expgdd_bbd$doy,predict(smbbdmod_targt), main="Target warming mod")
abline(lm(predict(smbbdmod_targt)~expgdd_bbd$doy), col="red")
abline(a=0, b=1, lwd=2)
summary(lm(predict(smbbdmod_targt)~expgdd_bbd$doy))
mean(expgdd_bbd$doy)
AIC(smbbdmod,smbbdmod_targt,smbbdmod_agtmax,smbbdmod2,smbbdmod1,smbbdmod1a)
#smbbdmod1a has lowest aic, highset r2 in pred vs obs. use this model in figure
