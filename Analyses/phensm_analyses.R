#Preliminary analyses of experimental phenology, soil moisture, and temperatur data for radcliffe
#Started March 18, 2016 by Ailene Ettinger
#Modified/added to by Ailene February-April 2017 for ESA abstract and additional analyses

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
## load packages
library(RColorBrewer)
library(lme4)
library(car)
library(dplyr)
library(AICcmodavg)
update.packages()

##Read in experimental climate and phenology data
setwd("~/git/radcliffe")

expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)
expclim2<-full_join(treats,expclim, by=c("site", "block", "plot","temptreat","preciptreat"), match="all")

#Get one variable for aboveground warming (could be surface, canopy, air)
expclim2$agtemp_min<-expclim2$airtemp_min
expclim2[which(is.na(expclim2$agtemp_min) & !is.na(expclim2$cantemp_min)),]$agtemp_min<-expclim2[which(is.na(expclim2$airtemp_min) & !is.na(expclim2$cantemp_min)),]$cantemp_min
expclim2[which(is.na(expclim2$agtemp_min) & !is.na(expclim2$surftemp_min)),]$agtemp_min<-expclim2[which(is.na(expclim2$agtemp_min) & !is.na(expclim2$surftemp_min)),]$surftemp_min
expclim2$agtemp_max<-expclim2$airtemp_max
expclim2[which(is.na(expclim2$agtemp_max) & !is.na(expclim2$cantemp_max)),]$agtemp_max<-expclim2[which(is.na(expclim2$airtemp_max) & !is.na(expclim2$cantemp_max)),]$cantemp_max
expclim2[which(is.na(expclim2$agtemp_max) & !is.na(expclim2$surftemp_max)),]$agtemp_max<-expclim2[which(is.na(expclim2$agtemp_max) & !is.na(expclim2$surftemp_max)),]$surftemp_max
#remove site 2 (chuine) because these are not real temp measurements
expclim2<-expclim2[-which(expclim2$site=="exp02"),]
expclim2[which(is.na(expclim2$block)),]$block<-"none"

#calculate mean above-ground temp
expclim2$agtemp_mn<-(expclim2$agtemp_max+expclim2$agtemp_min)/2
#summarize soil moisture and air temperature by plot, seasonally and annually.
#start by aggregating observed above-ground min and max and soil temperature by plot and year to get annual values
ag_max_plot<-aggregate(expclim2$agtemp_max, by=list(expclim2$site,expclim2$block,expclim2$plot,expclim2$target,expclim2$preciptreat_amt,expclim2$year), FUN=mean,na.rm=TRUE)
ag_min_plot<-aggregate(expclim2$agtemp_min, by=list(expclim2$site,expclim2$block,expclim2$plot,expclim2$target,expclim2$preciptreat_amt,expclim2$year), FUN=mean,na.rm=TRUE)
soilmois_plot<-aggregate(expclim2$soilmois1, by=list(expclim2$site,expclim2$block,expclim2$plot,expclim2$target,expclim2$preciptreat_amt,expclim2$year), FUN=mean,na.rm=TRUE)
#combine into one dataframe
tempsm_plots<-cbind(ag_max_plot,ag_min_plot$x,soilmois_plot$x)
colnames(tempsm_plots)<-c("site","block","plot","warm_treat","precip_treat","year","agtmax","agtmin","sm")
tempsm_plots<-tempsm_plots[order(tempsm_plots$site,tempsm_plots$block,tempsm_plots$plot,tempsm_plots$year),]

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

## Fit preliminary models to get estimate of growing degree days at phenological events
## in each plot/species/site
##Compare GDDcrit in different treatments
exppheno$genus.species<-paste(exppheno$genus,exppheno$species,sep=".")
exppheno[which(is.na(exppheno$block)),]$block<-"none"
#merge phenology data with experimental climate to get gdd crit
exp_gdd<-inner_join(exppheno,expclim, by=c("site", "block", "plot","year","doy"), match="all")
#select out just columns wanted to fit model with soil moisture and aboveground temp
exp_gdd2<-subset(exp_gdd,selec=c(site,block,plot,event,year,genus,species,genus.species,doy,temptreat,preciptreat,cumgdd_soil,numnas_soilgdd,cumgdd_air,numnas_airgdd))
#merge in target temp, precip treatment, and annual ag temp and soil moisture values
exp_gdd3<-left_join(exp_gdd2,tempsm_plots, by=c("site", "block", "plot","year"), match="all")

#Analyses started April 11, 2017
#Want to fit a model with soil moisture and above-ground temperature as predictors for doy of phenological event
#Start by looking at what studies have both SM and AG temp data
which(tapply(expall$agtemp_max,expall$site,mean,na.rm=T)>0)
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
plot(expall_bbd$agtemp_max,expall_bbd$soilmois1,type="p", pch=21,bg="gray", ylab="Soil moisture", xlab="Above-ground temperature (C)", bty="l")
abline(lm(expall_bbd$soilmois1~expall_bbd$agtemp_max), lwd=2)
mtext("r=-0.20,p<0.01",side=3, adj=1, line=-1)
cor(expall_subs$soilmois1,expall_subs$agtemp_max)#they're not correlated for bud burs only!
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
