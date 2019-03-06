#Started Sept 2017
#By Ailene
#Two questions to address:
#1) How do soil moisture and temperature affect doy of bud burst, leaf out, etc?
#2) How do warming and precip treatments affect soil moisture? (Make plots and fit models)

#Use brms/Stan to fit soil moisture- phenology model to radcliffe data 
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE
library(lme4)
library(rstan)
library(ggplot2)
library(shinystan)
library(bayesplot)

#library(rstanarm)
library(dplyr)
library(brms)
library(RColorBrewer)
library(plotrix)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#options(mc.cores = parallel::detectCores()) # added by Andrew
#update.packages()

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/radcliffe")}
#setwd("~/Documents/GitHub/radcliffe")#noaa
#Goal: Fit a multi-model to phenology (budburst) data with temperature, soil moisture, and 
#their interaction as explanatory variables.
#
###Now with the data
###try without ncp if treedepth issue doesn't solve things.

#source('Analyses/soilmoisture/savestan.R')

#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())

#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)
remove.conifers=TRUE
use.airtemp=TRUE

#standard data wrangling to get expclim2 for climate analyses and expgdd for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")

#summarize climate data by plot (annual and seasonal temp, soil mois), 
  #merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
#source("Analyses/soilmoisture/climsum_byplot_soiltoo.R")#doesn't work for some reason....
source("Analyses/source/climsum_byplot.R")
#Realized that I need to look at the microclimate data that as they are summarized for the phenology models (not the daily data)
#Prepare data for phenology models in stan#####
source("Analyses/source/stanprep_phenmods.R")

#look at relationship between soil moisture and aboveground temperature
sm.mod.target<-lmer(soilmois_janmar~target + (target|site)+(1|year), REML=FALSE, data=expgdd_bbd)
sm.mod.meas<-lmer(soilmois_janmar~ag_min_janmar+ (ag_min_janmar|site)+(1|year), REML=FALSE, data=expgdd_bbd)
#slightly stronger effect of measured agtemp than target
summary(sm.mod.target)
summary(sm.mod.meas)
fixef(sm.mod.target)
fixef(sm.mod.meas)
range(expgdd_bbd$soilmois_janmar)
range(expgdd_bbd$soilmois_aprjun)#lower soil moistures
range(expgdd_bbd$ag_min_janmar)
range(expgdd_bbd$ag_min_aprjun)

sm.mod.meas2<-lmer(soilmois_aprjun~ag_min_aprjun+ (ag_min_aprjun|site)+(1|year), REML=FALSE, data=expgdd_bbd)

#Is relationship between target warming or measured warming and soil moisture different between controls and warming treatments
expgdd_bbd$ttype<-"cont"
expgdd_bbd$ttype[as.numeric(expgdd_bbd$target)>0]<-"warm"
expgdd_lod$ttype<-"cont"
expgdd_lod$ttype[as.numeric(expgdd_lod$target)>0]<-"warm"

#Is relationship between target warming or measured warming and soil moisture different between controls and warming/precip treatments
expgdd_bbd$ptype<-"cont"
expgdd_bbd$ptype[expgdd_bbd$preciptreat_amt==50]<-"dry"
expgdd_bbd$ptype[expgdd_bbd$preciptreat_amt==150]<-"wet"
#fit models
#sm.mod.target2<-lmer(soilmois_janmar~target*ttype + (target*ttype|site)+(1|year), REML=FALSE, data=expgdd_bbd)
#above failed to coverge
#sm.mod.target2<-lmer(soilmois_janmar~target*ttype + (1|site)+(1|year), REML=FALSE, data=expgdd_bbd)
expgdd_bbd$ttype<-as.factor(expgdd_bbd$ttype)
sm.mod.ttype<-lmer(soilmois_janmar~ag_min_janmar*ttype + (ag_min_janmar*ttype|site)+(1|styear), REML=FALSE, data=expgdd_bbd)
summary(sm.mod.ttype)

sm.mod.ttype2<-lmer(soilmois_aprjun~ag_min_aprjun*ttype + (ag_min_aprjun*ttype|site)+(1|styear), REML=FALSE, data=expgdd_bbd)
summary(sm.mod.ttype2)

#Make a plot of soilmois~agtemp, with lines the length of measured climate
#different lines for each site
#first for jan-mar(bbd)
sites_cont<-rownames(ranef(sm.mod.ttype)$site)
sites_treat<-rownames(ranef(sm.mod.ttype)$site)

#different sites get different colors
cols <- brewer.pal(8,"Set3")

quartz(height=7, width=10)
par(mfrow=c(1,2))
plot(expgdd_bbd$ag_min_janmar[1],expgdd_bbd$soilmois_janmar[1],type="p", pch=21,col="white", ylab="Soil moisture", xlab="Above-ground temperature", bty="l", xlim=c(min(expgdd_bbd$ag_min_janmar, na.rm=TRUE)-.5,max(expgdd_bbd$ag_min_janmar, na.rm=TRUE)),ylim=c(0,0.5), main="BB sites: Jan-Mar")

#add lines for models fit to control plots
for(i in 1:length(sites_cont)){
  site.mod<-coef(sm.mod.ttype)$site[i,]
  xsitemax<-max(expgdd_bbd$ag_min_janmar[expgdd_bbd==sites_cont[i]], na.rm=TRUE) 
  xsitemin<-min(expgdd_bbd$ag_min_janmar[expgdd_bbd==sites_cont[i]], na.rm=TRUE) 
  ysitemax<-max(expgdd_bbd$soilmois_janmar[expgdd_bbd==sites_cont[i]], na.rm=TRUE) 
  ysitemin<-min(expgdd_bbd$soilmois_janmar[expgdd_bbd==sites_cont[i]], na.rm=TRUE) 
  ablineclip(a=as.numeric(site.mod[1]),b=as.numeric(site.mod[2]),x1=xsitemin,x2=xsitemax,y1=0,y2=1.0,col=cols[i])
  print(site.mod);
  print(xsitemax); print(xsitemin);
  
  #xvals<-as.vector(seq(xsitemin,xsitemax))
  #yvals<-as.numeric(site.mod[2])*(xvals)+as.numeric(site.mod[1])
  #lines(xvals,yvals,col=cols[i], lty=3)
}
#main line for control plots
xmax<-max(expgdd_bbd$ag_min_janmar, na.rm=TRUE) 
xmin<-min(expgdd_bbd$ag_min_janmar, na.rm=TRUE) 

ablineclip(fixef(sm.mod.ttype)[1:2],lwd=2,col="black", x1=xmin, x2=xmax)
#main line for treatment plots

ablineclip(c(fixef(sm.mod.ttype)[1]+fixef(sm.mod.ttype)[3],fixef(sm.mod.ttype)[2]+fixef(sm.mod.ttype)[4]),lwd=2,col="black", lty=2,x1=xmin, x2=xmax)

#now add site-level treatment plots
for(i in 1:length(sites_treat)){
  site.mod<-coef(sm.mod.ttype)$site[i,]
  xsitemax<-max(expgdd_bbd$ag_min_janmar[expgdd_bbd==sites_treat[i]], na.rm=TRUE) 
  xsitemin<-min(expgdd_bbd$ag_min_janmar[expgdd_bbd==sites_treat[i]], na.rm=TRUE) 
  ysitemax<-max(expgdd_bbd$soilmois_janmar[expgdd_bbd==sites_treat[i]], na.rm=TRUE) 
  ysitemin<-min(expgdd_bbd$soilmois_janmar[expgdd_bbd==sites_treat[i]], na.rm=TRUE) 
  ablineclip(a=as.numeric(site.mod[1]+site.mod[3]),b=as.numeric(site.mod[2]+site.mod[4]),x1=xsitemin,x2=xsitemax,y1=-0.5,y2=1.0,col=cols[i], lty=2)
  print(site.mod);
  print(xsitemax); print(xsitemin);
}


#now apr-jun
plot(expgdd_bbd$ag_min_aprjun[1],expgdd_bbd$soilmois_aprjun[1],type="p", pch=21,col="white", ylab="Soil moisture", xlab="Above-ground temperature", bty="l", xlim=c(min(expgdd_bbd$ag_min_aprjun, na.rm=TRUE)-.5,max(expgdd_bbd$ag_min_aprjun, na.rm=TRUE)),ylim=c(0,0.5), main="BB sites: Apr-Jun")

#add lines for models fit to control plots
for(i in 1:length(sites_cont)){
  site.mod<-coef(sm.mod.ttype2)$site[i,]
  xsitemax<-max(expgdd_bbd$ag_min_aprjun[expgdd_bbd==sites_cont[i]], na.rm=TRUE) 
  xsitemin<-min(expgdd_bbd$ag_min_aprjun[expgdd_bbd==sites_cont[i]], na.rm=TRUE) 
  ysitemax<-max(expgdd_bbd$soilmois_aprjun[expgdd_bbd==sites_cont[i]], na.rm=TRUE) 
  ysitemin<-min(expgdd_bbd$soilmois_aprjun[expgdd_bbd==sites_cont[i]], na.rm=TRUE) 
  ablineclip(a=as.numeric(site.mod[1]),b=as.numeric(site.mod[2]),x1=xsitemin,x2=xsitemax,y1=0,y2=1.0,col=cols[i])
  print(site.mod);
  print(xsitemax); print(xsitemin);
  
  #xvals<-as.vector(seq(xsitemin,xsitemax))
  #yvals<-as.numeric(site.mod[2])*(xvals)+as.numeric(site.mod[1])
  #lines(xvals,yvals,col=cols[i], lty=3)
}
#main line for control plots
xmax<-max(expgdd_bbd$ag_min_aprjun, na.rm=TRUE) 
xmin<-min(expgdd_bbd$ag_min_aprjun, na.rm=TRUE) 

ablineclip(fixef(sm.mod.ttype2)[1:2],lwd=2,col="black", x1=xmin, x2=xmax)
#main line for treatment plots
ablineclip(c(fixef(sm.mod.ttype2)[1]+fixef(sm.mod.ttype2)[3],fixef(sm.mod.ttype2)[2]+fixef(sm.mod.ttype2)[4]),lwd=2,col="black", lty=2,x1=xmin, x2=xmax)

#now add treatment plots
for(i in 1:length(sites_treat)){
  site.mod<-coef(sm.mod.ttype2)$site[i,]
  xsitemax<-max(expgdd_bbd$ag_min_aprjun[expgdd_bbd==sites_treat[i]], na.rm=TRUE) 
  xsitemin<-min(expgdd_bbd$ag_min_aprjun[expgdd_bbd==sites_treat[i]], na.rm=TRUE) 
  ysitemax<-max(expgdd_bbd$soilmois_aprjun[expgdd_bbd==sites_treat[i]], na.rm=TRUE) 
  ysitemin<-min(expgdd_bbd$soilmois_aprjun[expgdd_bbd==sites_treat[i]], na.rm=TRUE) 
  ablineclip(a=as.numeric(site.mod[1]+site.mod[3]),b=as.numeric(site.mod[2]+site.mod[4]),x1=xsitemin,x2=xsitemax,y1=-0.5,y2=1.0,col=cols[i], lty=2)
  print(site.mod);
  print(xsitemax); print(xsitemin);
}
sitnames<-unique(expgdd_bbd$site2)
legend("topright",legend=paste(sitnames),lwd=1,col=cols[1:length(sites_treat)], bty="n")
legend("topright",legend=c("control","treatment"),lwd=2,col="black", lty=c(1,2),bty="n")










expgdd_lod$ttype<-as.factor(expgdd_lod$ttype)

sm.mod.meas2.lod<-lmer(soilmois_janmar~ag_min_janmar*ttype + (1|site)+(1|year), REML=FALSE, data=expgdd_lod)

summary(sm.mod.meas2.lod)#strong interaction between
#no interaction between agmin and warming type (control vs warmed)
fixef(sm.mod.target2)
fixef(sm.mod.meas2)
#check some things:
unique(expgdd_bbd$site2)
unique(expgdd_bbd[expgdd_bbd$site2=="exp01",]$ptype)#checked target, ttype, ptype
unique(expgdd_bbd[expgdd_bbd$site2=="exp03",]$ptype)#only control for ptype
unique(expgdd_bbd[expgdd_bbd$site2=="exp03",]$preciptreat_amt)#only control for ptype

unique(expgdd_bbd[expgdd_bbd$site2=="exp04",]$ptype)
unique(expgdd_bbd[expgdd_bbd$site2=="exp04",]$preciptreat_amt)#only control for ptype

unique(expgdd_bbd[expgdd_bbd$site2=="exp07",]$ptype)
unique(expgdd_bbd[expgdd_bbd$site2=="exp07",]$preciptreat_amt)#only control for ptype

unique(expgdd_bbd[expgdd_bbd$site2=="exp10",]$ptype)
unique(expgdd_bbd[expgdd_bbd$site2=="exp10",]$preciptreat_amt)#only control for ptype



expgdd_bbd$ag_min_janmar, #above-ground minimum air temp = temp in bb mods
expgdd_bbd$soilmois_janmar#soil moisture



####Old code
#1) How do warming and precip treatments affect temperature and soil moisture? (Make plots and fit models)

expclim2$target_cent<-scale(expclim2$target, center = TRUE, scale = TRUE)
expclim2$preciptreat_amt_cent<-scale(expclim2$preciptreat_amt, center = TRUE, scale = TRUE)
expclim2a<-subset(expclim2,select=c(site,year,doy,target_cent,preciptreat_amt,target,preciptreat_amt_cent,soilmois1,agtemp_mean))
expclim2a<- expclim2a [apply(expclim2a , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
expclim2a$doy<-as.factor(expclim2a$doy)
expclim2a$year<-as.factor(expclim2a$year)
expclim2a$site<-as.factor(expclim2a$site)
expclim2a$preciptreat_prop<-expclim2a$preciptreat_amt/100

#select out only sites that manip both temp and precip
expclim3<-expclim2a[expclim2a$site=="exp01"|expclim2a$site=="exp05"|expclim2a$site=="exp09"|expclim2a$site=="exp12",]
#expclim3$target_cent<-scale(expclim3$target, center = TRUE, scale = TRUE)
#expclim3$preciptreat_amt_cent<-scale(expclim3$preciptreat_amt, center = TRUE, scale = TRUE)
expclim3<- expclim3 [apply(expclim3 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na

#fit model in lmer
###Fit lmer model for soil moisture~warming*preciptreatment
sm_mod<-lmer(soilmois1~target*preciptreat_prop + (target*preciptreat_prop|site)+(1|year/doy), REML=FALSE, data=expclim3)
summary(sm_mod)#doesn't converge for preciptreat_amt or precipttreat_prop with expclim2a but DOES for expclim3

smtemp_mod<-lmer(soilmois1~target + (target|site)+(1|year/doy), REML=FALSE, data=expclim2a)
summary(smtemp_mod)

smprecip_mod<-lmer(soilmois1~preciptreat_prop + (preciptreat_prop|site)+(1|year/doy), REML=FALSE, data=expclim2a)
summary(smprecip_mod)

#sm_mod_cent<-lmer(soilmois1~target_cent*preciptreat_amt_cent + 
#                    (target_cent*preciptreat_amt_cent|site)+(1|year/doy),
 #                 REML=FALSE, data=expclim2a)
#summary(sm_mod_cent)#convergence warning for both expclim3 and expclim 2a

#sm_mod_cent.b<-brm(soilmois1~target_cent*preciptreat_amt_cent + 
 #             ((target_cent*preciptreat_amt_cent)|site)+(1|year/doy),
  #            data=expclim3)#
sm_mod.b<-brm(soilmois1~target*preciptreat_prop + 
                     (target*preciptreat_prop|site)+(1|year/doy),
                   data=expclim3,
              chains = 2)#

smtemp_mod.b<-brm(soilmois1~target + 
                     (target|site)+(1|year/doy),
                   data=expclim2a)#
smprecip_mod.b<-brm(soilmois1~preciptreat_prop + 
                    (preciptreat_prop|site)+(1|year/doy),
                  data=expclim2a)#

#summary(sm_mod_cent.b)
#temp mod
temp_mod_cent<-lmer(agtemp_mean ~ target_cent*preciptreat_amt_cent +#fixed effects
                       (target_cent*preciptreat_amt_cent|site) + (1|year/doy), #random effects
                     data=expclim2a)# control = list(max_treedepth = 15,adapt_delta = 0.99)

#fit model with brms
temp_mod.b<-brm(agtemp_mean ~ target*preciptreat_prop +#fixed effects
      (target*preciptreat_prop|site) + (1|year/doy), #random effects
    data=expclim3,
    chains = 2)# control = list(max_treedepth = 15,adapt_delta = 0.99)

summary(sm_mod_cent.b)
#save coefficients from models to make a table
#table<-cbind(fixef(temp_mod_cent.b),fixef(sm_mod_cent.b))
table<-cbind(summary(temp_mod_cent)$coefficients[,1:2],summary(sm_mod_cent)$coefficients[,1:2])
table<-round(table, digits=3)
#sm_tempmod_cent<-lmer(soilmois1~target_cent + (target_cent|site)+(1|year/doy), REML=FALSE, data=expclim2a)
#summary(sm_tempmod_cent)#model does not fit for noncentered data
#AIC(sm_tempmod_cent,sm_mod_cent)
colnames(table)<-c("Tmod.coef","Tmod.se","SMmod.coef","SMmod.se")
rownames(table)<-c("int","temp.treat","precip.treat","temp.treat*precip.treat")
write.csv(table,"Analyses/soilmoisture/tempmoislmer.csv", row.names = TRUE)


#Figure with lines for each site showing different effects
quartz(height=7, width=7)
par(mfrow=c(2,2))
#first air temp
plot(expclim2a$target_cent[1],expclim2a$agtemp_mean[1],type="p", pch=21,col="white", ylab="Above-ground temperature", xlab="Target warming (centered)", bty="l", xlim=c(-1.5,2.5),ylim=c(0,30))
#site effects
cols <- brewer.pal(12,"Set3")

for(i in 1:dim(coef(temp_mod_cent)$site)[1]){
  abline(a=coef(temp_mod_cent)$site[i,1],b=coef(temp_mod_cent)$site[i,2], lwd=1, col= cols[i])
}
#main effects
abline(a=fixef(temp_mod_cent)[1],b=fixef(temp_mod_cent)[2], lwd=2)

plot(expclim2a$preciptreat_amt_cent[1],expclim2a$agtemp_mean[1],type="p", pch=21,col="white", ylab="Above-ground temperature", xlab="Precipitation treatment (centered)", bty="l", xlim=c(-2.5,5),ylim=c(0,30))
#site effects
for(i in 1:dim(coef(temp_mod_cent)$site)[1]){
  abline(a=coef(temp_mod_cent)$site[i,1],b=coef(temp_mod_cent)$site[i,3], lwd=1, col= cols[i])
}
abline(a=fixef(temp_mod_cent)[1],b=fixef(temp_mod_cent)[3], lwd=2)

#soil moisture
plot(expclim2a$target_cent[1],expclim2$soilmois1[1],type="p", pch=21,col="white", ylab="Soil moisture", xlab="Target warming (centered)", bty="l", ylim=c(0,0.4),xlim=c(-1.5,2.5))

for(i in 1:dim(coef(sm_mod_cent)$site)[1]){
abline(a=coef(sm_mod_cent)$site[i,1],b=coef(sm_mod_cent)$site[i,2], lwd=1, col= cols[i])
}
#main effects
abline(a=fixef(sm_mod_cent)[1],b=fixef(sm_mod_cent)[2], lwd=2)

plot(expclim2a$preciptreat_amt_cent[1],expclim2a$soilmois1[1],type="p", pch=21,col="white", ylab="Soil moisture", xlab="Precipitation treatment (centered)", bty="l", xlim=c(-2.5,5),ylim=c(0,0.4))
#site effects
for(i in 1:dim(coef(sm_mod_cent)$site)[1]){
  abline(a=coef(sm_mod_cent)$site[i,1],b=coef(sm_mod_cent)$site[i,3], lwd=1, col= cols[i])
}
#main effects
abline(a=fixef(sm_mod_cent)[1],b=fixef(sm_mod_cent)[3], lwd=2)
#Need to add interaction somehow..
sites<-unique(expclim2a$site)
op<-par(cex=.4)
legend("bottomright",legend=paste(sites),lwd=1,col=cols[1:length(sites)], bty="n")

expclim2$preciptreat_prop<-expclim2$preciptreat_amt/100
#now fit a model of
#target warming:
sm_mod_targ<-lmer(soilmois1~target*preciptreat_prop + (target*preciptreat_prop|site)+(1|year/doy), REML=FALSE, data=expclim2)
summary(sm_mod_targ)

#measured warming:
sm_mod_meas<-lmer(soilmois1~agtemp_mean*preciptreat_prop + (agtemp_mean*preciptreat_prop|site)+(1|year/doy), REML=FALSE, data=expclim2)
summary(sm_mod_meas)
###########################
############################
#Fit models of soilmois~measured warming with predictor of treatment type as well 
#use measured warming and predictor for treatment (control, warmed only, warmedwith precip)
###########################
###########################
#create a new varibale for treatment type (ttype)
expclim2$ttype<-"cont"
expclim2$ttype[as.numeric(expclim2$temptreat)>0]<-"warmprecip"
expclim2$ttype[expclim2$preciptreat==-1 ]<-"warmprecip"
expclim2$ttype[expclim2$preciptreat==1]<-"warmprecip"
expclim2$ttype2<-"cont"
expclim2$ttype2[as.numeric(expclim2$temptreat)>0]<-"warm"
expclim2$ttype2[expclim2$preciptreat==-1 ]<-"precip"
expclim2$ttype2[expclim2$preciptreat==1]<-"precip"

#do we want to separate out precip only treatments?
expclim2$ttype<-as.factor(expclim2$ttype)
expclim2$ttype2<-as.factor(expclim2$ttype2)

expclim2$agtemp_mean<-as.numeric(expclim2$agtemp_mean)
expclim2$soilmois1<-as.numeric(expclim2$soilmois1)
#divide into 2 different datasets
expclim_cont<-expclim2[expclim2$ttype=="cont",]
expclim_treat<-expclim2[expclim2$ttype=="warmprecip",]
#remove exp02, which doesn't measure airtemp
expclim_cont<-expclim_cont[!expclim_cont$site=="exp02",]
expclim_treat<-expclim_treat[!expclim_treat$site=="exp02",]

#expclim_precip<-expclim2[expclim2$ttype=="precip",]

sm_mod_temptreat<-lmer(soilmois1~agtemp_mean*ttype + (agtemp_mean*ttype|site)+(1|year/doy), REML=FALSE, data=expclim2)
summary(sm_mod_temptreat)#no convergence
#Fit separate model for each treatment type for now?

sm_mod_temptreat_cont<-lmer(soilmois1~agtemp_mean+ (agtemp_mean|site)+(1|year/doy), REML=FALSE, data=expclim_cont)
summary(sm_mod_temptreat_cont)

sm_mod_temptreat_treat<-lmer(soilmois1~agtemp_mean + (agtemp_mean|site)+(1|year/doy), REML=FALSE, data=expclim_treat)
summary(sm_mod_temptreat_treat)


#sm_mod_temp.brms<-brm(soilmois1~agtemp_mean 
#                      + (agtemp_mean|site)+(1|year/doy), 
 #                     data=expclim2)
#summary(sm_mod_temp.brms)

#Make a plot of soilmois~agtemp, with lines the length of measured climate
#different lines for each site
sites_cont<-rownames(ranef(sm_mod_temptreat_cont)$site)
sites_treat<-rownames(ranef(sm_mod_temptreat_treat)$site)

#the sites are the same
#different sites get different colors
cols <- brewer.pal(8,"Set3")

quartz(height=7, width=7)
plot(expclim_cont$target_cent[1],expclim_cont$agtemp_mean[1],type="p", pch=21,col="white", ylab="Soil moisture", xlab="Above-ground temperature", bty="l", xlim=c(min(expclim2$agtemp_mean, na.rm=TRUE)-.5,max(expclim2$agtemp_mean, na.rm=TRUE)),ylim=c(-0,0.5))

#add lines for models fit to control plots
for(i in 1:length(sites_cont)){
 site.mod<-coef(sm_mod_temptreat_cont)$site[i,]
 xsitemax<-max(expclim_cont$agtemp_mean[expclim_cont==sites_cont[i]], na.rm=TRUE) 
 xsitemin<-min(expclim_cont$agtemp_mean[expclim_cont==sites_cont[i]], na.rm=TRUE) 
 ysitemax<-max(expclim_cont$soilmois1[expclim_cont==sites_cont[i]], na.rm=TRUE) 
 ysitemin<-min(expclim_cont$soilmois1[expclim_cont==sites_cont[i]], na.rm=TRUE) 
 ablineclip(a=as.numeric(site.mod[1]),b=as.numeric(site.mod[2]),x1=xsitemin,x2=xsitemax,y1=-0.5,y2=1.0,col=cols[i])
 print(site.mod);
 print(xsitemax); print(xsitemin);
 
 #xvals<-as.vector(seq(xsitemin,xsitemax))
 #yvals<-as.numeric(site.mod[2])*(xvals)+as.numeric(site.mod[1])
 #lines(xvals,yvals,col=cols[i], lty=3)
}
#main line for control plots
xmax<-max(expclim_cont$agtemp_mean, na.rm=TRUE) 
xmin<-min(expclim_cont$agtemp_mean, na.rm=TRUE) 

ablineclip(fixef(sm_mod_temptreat_cont),lwd=2,col="black", x1=xmin, x2=xmax)
#now add treatment plots
for(i in 1:length(sites_treat)){
  site.mod<-coef(sm_mod_temptreat_treat)$site[i,]
  xsitemax<-max(expclim_treat$agtemp_mean[expclim_treat==sites_treat[i]], na.rm=TRUE) 
  xsitemin<-min(expclim_treat$agtemp_mean[expclim_treat==sites_treat[i]], na.rm=TRUE) 
  ysitemax<-max(expclim_treat$soilmois1[expclim_treat==sites_treat[i]], na.rm=TRUE) 
  ysitemin<-min(expclim_treat$soilmois1[expclim_treat==sites_treat[i]], na.rm=TRUE) 
  ablineclip(a=as.numeric(site.mod[1]),b=as.numeric(site.mod[2]),x1=xsitemin,x2=xsitemax,y1=-0.5,y2=1.0,col=cols[i], lty=2)
  print(site.mod);
  print(xsitemax); print(xsitemin);
}




xmax2<-max(expclim_treat$agtemp_mean, na.rm=TRUE) 
xmin2<-min(expclim_treat$agtemp_mean, na.rm=TRUE) 

ablineclip(fixef(sm_mod_temptreat_treat),lwd=2,col="black", lty=2, x1=xmin2, x2=xmax2)
###############
#Try divide into 3 different datasets
expclim_cont2<-expclim2[expclim2$ttype2=="cont",]
expclim_warm<-expclim2[expclim2$ttype2=="warm",]

#remove exp02, which doesn't measure airtemp
expclim_cont2<-expclim_cont2[!expclim_cont2$site=="exp02",]
expclim_warm<-expclim_warm[!expclim_warm$site=="exp02",]





#########Exploring#########

sm_mod_temp_cont<-lmer(soilmois1~agtemp_mean + (agtemp_mean|site)+(1|year/doy), REML=FALSE, data=expclim_cont)
summary(sm_mod_temp_cont)

modcomp<-rbind(fixef(sm_mod_targ)[,1:2],fixef(sm_mod_meas)[,1:2],c(fixef(sm_mod_temp)[,1],NA,NA),c(fixef(sm_mod_temp)[,2],NA,NA),c(fixef(sm_mod_temp_cont)[,1],NA,NA),c(fixef(sm_mod_temp_cont)[,2],NA,NA))

colnames(modcomp)<-c("eff","se")
