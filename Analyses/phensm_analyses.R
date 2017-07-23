#Preliminary analyses of experimental phenology, soil moisture, and temperatur data for radcliffe
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
library(car)
library(dplyr)
library(AICcmodavg)
#update.packages()

##Read in experimental climate and phenology data
setwd("~/git/radcliffe")
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)
# sourcing standard wrangling to get expclim2 and expgdd for climate and phenology analyses, respectively
source("source/standard_mergesandwrangling.R")

#Need to summarize soil moisture and air temperature by plot, seasonally and annually and merge this in to climate data
#start by aggregating observed above-ground min and max and soil temperature by plot and year to get annual values
ag_max_plot<-aggregate(expclim2$agtemp_max, by=list(expclim2$site,expclim2$block,expclim2$plot,expclim2$target,expclim2$preciptreat_amt,expclim2$year), FUN=mean,na.rm=TRUE)
ag_min_plot<-aggregate(expclim2$agtemp_min, by=list(expclim2$site,expclim2$block,expclim2$plot,expclim2$target,expclim2$preciptreat_amt,expclim2$year), FUN=mean,na.rm=TRUE)
soilmois_plot<-aggregate(expclim2$soilmois1, by=list(expclim2$site,expclim2$block,expclim2$plot,expclim2$target,expclim2$preciptreat_amt,expclim2$year), FUN=mean,na.rm=TRUE)
#combine into one dataframe
tempsm_plots<-cbind(ag_max_plot,ag_min_plot$x,soilmois_plot$x)
colnames(tempsm_plots)<-c("site","block","plot","target","preciptreat_amt","year","agtmax","agtmin","sm")
tempsm_plots<-tempsm_plots[order(tempsm_plots$site,tempsm_plots$block,tempsm_plots$plot,tempsm_plots$year),]
dim(tempsm_plots)#894 9
dim(expclim2)#219493     53
expclim3<-full_join(expclim2,tempsm_plots,by=c("site", "block", "plot","target","preciptreat_amt","year"), match="all")
dim(expclim3)#219493     53
dim(exppheno)#69008      53

#Now merge in phenology data to get
expall<-left_join(exppheno,expclim,by=c("site", "block", "plot","year","doy"), copy=TRUE)
dim(expall)#69030    55
head(expall)
tail(expall)
length(which(is.na(expall$site)))#0- good
length(which(is.na(expall$airtemp_min)))#21933 
length(which(is.na(expall$soilmois1)))#16968 not good!too many
#for some reason, the merge works much better on expclim than on expclim2, so we'll have to do same manipulations to data

#First, 
#1) How do warming and precip treatments affect soil moisture? (Make plots and fit models)

#fit model in lmer
###Fit lmer model for soil moisture~warming*preciptreatment
expclim2$target_cent<-scale(expclim2$target, center = TRUE, scale = TRUE)
expclim2$preciptreat_amt_cent<-scale(expclim2$preciptreat_amt, center = TRUE, scale = TRUE)
expclim2a<-subset(expclim2,select=c(site,year,doy,target_cent,preciptreat_amt_cent,soilmois1))
expclim2a  <- expclim2a [apply(expclim2a , 1, function(x) all(!is.na(x))),] # only keep rows of all not na

sm_mod_cent<-lmer(soilmois1~target_cent*preciptreat_amt_cent + (1|site)+ (1|year/doy), REML=FALSE, data=expclim2)
summary(sm_mod_cent)

sm_mod_cent<-lmer(soilmois1~target_cent*preciptreat_amt_cent + (1|site/year/doy), REML=FALSE, data=expclim2)
summary(sm_mod_cent)

sm_mod<-lmer(soilmois1~target*preciptreat_amt + (1|site/year/doy), REML=FALSE, data=expclim2)
summary(sm_mod)
#sm_mod2<-lmer(soilmois1~target*preciptreat_amt + (target*preciptreat_amt|site/year/doy), REML=FALSE, data=expclim2)
#summary(sm_mod2)
quartz(height=5, width=9)
par(mfrow=c(1,2))
plot(expclim2$target,expclim2$soilmois1,type="p", pch=21,bg="lightgray",col="gray", ylab="Soil moisture", xlab="Target warming (C)", bty="l", ylim=c(0,0.8))
abline(a=fixef(sm_mod)[1],b=fixef(sm_mod)[2], lwd=2)
temp_slope=round(fixef(sm_mod)[2], digits=4)
temp_se=round(summary(sm_mod)$coefficients[2,2], digits=3)
mtext(paste("coef=",temp_slope), side=3, line=-4, adj=.8)

plot(expclim2$preciptreat_amt,expclim2$soilmois1,type="p", pch=21,bg="lightgray",col="gray", ylab="Soil moisture", xlab="Precipitation treatment (%)", bty="l", ylim=c(0,0.8))
abline(a=fixef(sm_mod)[1],b=fixef(sm_mod)[3], lwd=2)
prec_slope=round(fixef(sm_mod)[3], digits=4)
prec_se=round(summary(sm_mod)$coefficients[3,3], digits=4)
mtext(paste("coef=",prec_slope), side=3, line=-4, adj=.8)

#Make the above figure but without raw data- just site differences (random effects) plotted
#plot fitted lines, points, and 1:1 line

sites<-unique(expclim2$site)
expclim2$target<-as.numeric(expclim2$target)
soilmois_target<-aggregate(expclim2$soilmois1, by=list(expclim2$site,expclim2$target), FUN=mean,na.rm=TRUE)
soilmois_precip<-aggregate(expclim2$soilmois1, by=list(expclim2$site,expclim2$preciptreat_amt), FUN=mean,na.rm=TRUE)
colnames(soilmois_target)<-c("site","target","soilmois1")
colnames(soilmois_precip)<-c("site","precip_amt","soilmois1")

quartz(height=5, width=9)
par(mfrow=c(1,2))
plot(soilmois_target$target,soilmois_target$soilmois1,type="p",pch=as.numeric(as.factor(soilmois_target$site))+14, col="black",bg="black",xlab="Target warming (C)", ylab="Soil moisture (VWC)", bty="l", xlim=c(0,6), ylim=c(0,0.4))
abline(a=fixef(sm_mod)[1],b=fixef(sm_mod)[2], lwd=2)
temp_slope=round(fixef(sm_mod)[2], digits=4)
temp_se=round(summary(sm_mod)$coefficients[2,2], digits=3)
#mtext(paste("coef=",temp_slope), side=3, line=-4, adj=.8)

plot(soilmois_precip$precip,soilmois_precip$soilmois1,type="p",pch=as.numeric(as.factor(soilmois_target$site))+14, col="black",bg="black",xlab="Precipitation treatment (%)", ylab="Soil moisture (VWC)", bty="l", xlim=c(0,200), ylim=c(0,0.4))
abline(a=fixef(sm_mod)[1],b=fixef(sm_mod)[3], lwd=2)
prec_slope=round(fixef(sm_mod)[3], digits=4)
prec_se=round(summary(sm_mod)$coefficients[3,3], digits=4)
#mtext(paste("coef=",prec_slope), side=3, line=-4, adj=.8)

#Analyses started April 11, 2017
#Want to fit a model with soil moisture and above-ground temperature as predictors for doy of phenological event
#Start by looking at which studies have both SM and AG temp data

which(tapply(expall$agtemp_mn,expall$site,mean,na.rm=T)>0)
which(tapply(expall$soiltemp1_min,expall$site,mean,na.rm=T)>0)
which(tapply(expall$soilmois1,expall$site,mean,na.rm=T)>0)
#The following sites have both: exp01 exp02 exp03 exp04 exp07 exp09 exp10 exp12 
expall_subs<-expall[which(expall$site=="exp01"|expall$site=="exp02"|expall$site=="exp03"|expall$site=="exp04"|expall$site=="exp07"|expall$site=="exp09"|expall$site=="exp10"|expall$site=="exp12"),]#
expall_subs<-subset(expall_subs,select=c(doy,soilmois1,agtemp_max,agtemp_min,site,year,genus.species, event))
expall_subs <- expall_subs[apply(expall_subs, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#scale variables:
expall_subs$soilmois1_cent<-scale(expall_subs$soilmois1, center = TRUE, scale = TRUE)
expall_subs$agtemp_min_cent<-scale(expall_subs$agtemp_min, center = TRUE, scale = TRUE)
expall_subs$agtemp_max_cent<-scale(expall_subs$agtemp_max, center = TRUE, scale = TRUE)
#Figure out the random effects structure (random slopes vs intecepts:
sm_doymod1<-lmer(doy~agtemp_max_cent*soilmois1_cent + (1|site/genus.species)+ (1|year), REML=TRUE, data=expall_subs)
sm_doymod2<-lmer(doy~agtemp_max_cent*soilmois1_cent + (agtemp_max_cent*soilmois1_cent|site/genus.species)+ (1|year), REML=TRUE, data=expall_subs)
AIC(sm_doymod1,sm_doymod2)#mod2 wins, so this is our working model
sm_doymod<-lmer(doy~agtemp_max_cent*soilmois1_cent + (agtemp_max_cent*soilmois1_cent|site/genus.species)+ (1|year), REML=FALSE, data=expall_bbd)
sm_doymodA<-lmer(doy~agtemp_max_cent + (agtemp_max_cent|site/genus.species)+ (1|year), REML=FALSE, data=expall_bbd)
sm_doymodS<-lmer(doy~soilmois1_cent + (soilmois1_cent|site/genus.species)+ (1|year), REML=FALSE, data=expall_bbd)
sm_doymodAS<-lmer(doy~agtemp_max_cent+soilmois1_cent + (agtemp_max_cent+soilmois1_cent|site/genus.species)+ (1|year), REML=FALSE, data=expall_bbd)
AIC(sm_doymod,sm_doymodA,sm_doymodS,sm_doymodAS)#mod2 wins, so this is our working model
summary(sm_doymod)

#I think it only makes sense to fit models for particular events- start with bbd, lod and lud since these are the most common.
expall_bbd<-expall_subs[which(expall_subs$event=="bbd"),]#
expall_bbd$soilmois1_cent<-scale(expall_bbd$soilmois1, center = TRUE, scale = TRUE)
expall_bbd$agtemp_min_cent<-scale(expall_bbd$agtemp_min, center = TRUE, scale = TRUE)
expall_bbd$agtemp_max_cent<-scale(expall_bbd$agtemp_max, center = TRUE, scale = TRUE)
expall_lod<-expall_subs[which(expall_subs$event=="lod"),]#
expall_lod$soilmois1_cent<-scale(expall_lod$soilmois1, center = TRUE, scale = TRUE)
expall_lod$agtemp_min_cent<-scale(expall_lod$agtemp_min, center = TRUE, scale = TRUE)
expall_lod$agtemp_max_cent<-scale(expall_lod$agtemp_max, center = TRUE, scale = TRUE)
expall_lud<-expall_subs[which(expall_subs$event=="lud"),]#
expall_lud$soilmois1_cent<-scale(expall_lud$soilmois1, center = TRUE, scale = TRUE)
expall_lud$agtemp_min_cent<-scale(expall_lud$agtemp_min, center = TRUE, scale = TRUE)
expall_lud$agtemp_max_cent<-scale(expall_lud$agtemp_max, center = TRUE, scale = TRUE)

smbbdmod<-lmer(doy~agtemp_max_cent*soilmois1_cent + (agtemp_max_cent*soilmois1_cent|site/genus.species)+ (1|year), REML=FALSE, data=expall_bbd)
smlodmod<-lmer(doy~agtemp_max_cent*soilmois1_cent + (agtemp_max_cent*soilmois1_cent|site/genus.species)+ (1|year), REML=FALSE, data=expall_lod)
smludmod<-lmer(doy~agtemp_max_cent*soilmois1_cent + (agtemp_max_cent*soilmois1_cent|site/genus.species)+ (1|year), REML=FALSE, data=expall_lud)
summary(smbbdmod)
summary(smlodmod)
summary(smludmod)


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

quartz(height=6,width=10)
par(mfrow=c(1,2))
plot(expall_lud$agtemp_max,expall_lud$doy,type="p", pch=21,bg="gray", ylab="Leaf unfolding DOY", xlab="Above-ground temperature (C)", bty="l")
plot(expall_lud$soilmois1,expall_lud$doy,type="p", pch=21,bg="gray", ylab="Leaf unfolding DOY", xlab="Soil moisture", bty="l")

#Ok, so the problem is that there are natural pattersn in soil moisture and air temperature throughout the year so this will not work.
#First, look at relationahip between soil moisture and aboveground temperature in control plots
#Just use climate data for this, not phenology data
head(expclim2)
unique(expclim2$target)
tail(expclim2[which(is.na(expclim2$target)),])
table(expclim2$temptreat,expclim2$target)
expclim_noprecip<-expclim2
expall_cont<-expclim2[which(expclim2$temptreat=="ambient"|expclim2$temptreat=="0"),]#select controls
expall_cont<-expall_cont[!expall_cont$preciptreat=="-1",]#remove precip treatments
expall_cont<-expall_cont[!expall_cont$preciptreat=="1",]#remove precip treatments
#Now fit relationhip between agtemp and soil moisture for controls
quartz()
plot(expall_cont$agtemp_max,expall_cont$soilmois1,type="p", pch=21,bg="gray", ylab="Soil moisture", xlab="Above-ground temperature (C)", bty="l")
r=round(cor.test(expall_cont$soilmois1,expall_cont$agtemp_max)$estimate,digits=2)
p=round(cor.test(expall_cont$soilmois1,expall_cont$agtemp_max)$p.value,digits=3)
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

###Try fitting a growing degree day model for bbd
expall_subs2<-expall[which(expall$site=="exp01"|expall$site=="exp02"|expall$site=="exp03"|expall$site=="exp04"|expall$site=="exp07"|expall$site=="exp09"|expall$site=="exp10"|expall$site=="exp12"),]#
expall_subs2<-subset(expall_subs2,select=c(doy,soilmois1,agtemp_max,agtemp_min,cumgdd_air,site,year,genus.species, event))
expall_subs2 <- expall_subs2[apply(expall_subs2, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#scale variables:
expall_subs2$soilmois1_cent<-scale(expall_subs2$soilmois1, center = TRUE, scale = TRUE)
expall_subs2$agtemp_min_cent<-scale(expall_subs2$agtemp_min, center = TRUE, scale = TRUE)
expall_subs2$agtemp_max_cent<-scale(expall_subs2$agtemp_max, center = TRUE, scale = TRUE)
expall_bbd2<-expall_subs2[which(expall_subs2$event=="bbd"),]#
expall_bbd2$soilmois1_cent<-scale(expall_bbd2$soilmois1, center = TRUE, scale = TRUE)

gddbbdmod<-lmer(cumgdd_air~soilmois1_cent + (soilmois1_cent|site/genus.species)+ (1|year), REML=FALSE, data=expall_bbd2)
summary(gddbbdmod)

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

###Fit model of GDDcrit~soil moisture
#write.csv(exp_gdd3,"exphengdd.csv")




