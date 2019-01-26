#Started Sept 2017
#By Ailene
#Two questions to address:
#1) How do soil moisture and temperature affect doy of bud burst, leaf out, etc?
#2) How does accounting for effects of soil moisture alter forecasts of phenology?

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
library(rstanarm)
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

#source('Analyses/soilmoisture/savestan.R')

#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())

#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)

#Make some choices about how to restrict the data:
remove.conifers=TRUE
use.airtemp=TRUE
#standard data wrangling to get expclim2 for climate analyses and expgdd for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")

#Summarize climate data by plot (annual and seasonal temp, soil mois), 
#merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
#source("Analyses/soilmoisture/climsum_byplot_soiltoo.R")#doesn't work for some reason....
source("Analyses/source/climsum_byplot.R")

#Prepare data for phenology models in stan#####
source("Analyses/source/stanprep_phenmods.R")

##################
# Fit m5 to data #
##################

# testm5.lmer<-lmer(y~temp * mois +
#                     (temp*mois|sp)+ (1|site/year),
#                   REML=FALSE,
#                   data=datalist.bbd)
# summary(testm5.lmer)
#fails to converge wihtout conifers

# Anova(testm5.lmer)
# #model fits! a=108.961,temp= -3.734; mois=--33.070; temp:mois= 2.954
# #try adding year as a fixed effect
# testm5yr.lmer<-lmer(y~temp * mois * year +
#                       (temp*mois |sp)+ (1|site/year),
#                     data=datalist.bbd)#lower aic, interactions between year and temp and year and mois are significant
# 
# testm5yr2.lmer<-lmer(y~temp * mois * year +
#                        (temp*mois* year |sp)+ (1|site/year),
#                     data=datalist.bbd)#doesn't converge
# AIC(testm5.lmer,testm5yr.lmer)
# 
 testm5cent.lmer<-lmer(y~temp * mois +
                         (temp*mois|sp)+ (1|site/year),
                       REML=FALSE,
                      data=datalist.bbd.cent)
summary(testm5cent.lmer)
Anova(testm5cent.lmer)
# #model fits! a=99.4208, temp= -10.1869; mois=--1.2633; temp:mois=-0.3990
# testm5cont.lmer<-lmer(y~temp * mois +
#                         (temp*mois|sp)+ (1|site/year),
#                       data=datalist.bbdcont)
# summary(testm5cont.lmer)
# testm5contcent.lmer<-lmer(y~temp * mois +
#                             (temp*mois|sp)+ (1|site/year),
#                           data=datalist.bbdcont.cent)
# summary(testm5contcent.lmer)
# 
# #compare coefs from full dataset and control dataset
# comp.coefs.lmer<-cbind(round(fixef(testm5.lmer), digits=3),round(fixef(testm5cont.lmer), digits=3),round(fixef(testm5cent.lmer), digits=3),round(fixef(testm5contcent.lmer), digits=3))
# colnames(comp.coefs.lmer)<-c("m5","m5cont","m5.cent","m5cont.cent")
# write.csv(comp.coefs.lmer,"Analyses/soilmoisture/comp.coefs.lmer.csv", row.names = TRUE)
# Anova(testm5contcent.lmer)
# Anova(testm5cent.lmer)
# Anova(testm5cont.lmer)
# Anova(testm5.lmer)
# 
# #compare coefs in with soil temp versus air temp
# colnames(expgdd)
# #mois not significant in controls; is marginally significant in full dataset
# #Make plot of range of soil moisture in controls and in all
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# hist(datalist.bbd$mois, main="All plots")
# mn.mois<-round(mean(datalist.bbd$mois), digits=4)
# md.mois<-round(median(datalist.bbd$mois), digits=4)
# 
# var(datalist.bbd$mois)
# mtext(paste(c(mn.mois)))
# 
# hist(datalist.bbdcont$mois, main = "Control plots")
# mn.mois.cont<-round(mean(datalist.bbdcont$mois), digits=4)
# md.mois.cont<-round(median(datalist.bbdcont$mois), digits=4)
# 
# mtext(paste(mn.mois.cont))
# 
# cor(datalist.bbd$temp,datalist.bbd$mois)
# 
# #Now look ata temperature
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# hist(datalist.bbd$temp, main="All plots")
# mn.temp<-round(mean(datalist.bbd$temp), digits=4)
# md.temp<-round(median(datalist.bbd$temp), digits=4)
# 
# var(datalist.bbd$temp)
# mtext(paste(c(mn.temp)))
# 
# hist(datalist.bbdcont$temp, main = "Control plots")
# mn.temp.cont<-round(mean(datalist.bbdcont$temp), digits=4)
# md.temp.cont<-round(median(datalist.bbdcont$temp), digits=4)
# 
# mtext(paste(mn.temp.cont))

#look at bbdoy

#Now look at temperature
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# hist(datalist.bbd$y, main="All plots")
# mn.doy<-round(mean(datalist.bbd$y), digits=4)
# md.doy<-round(median(datalist.bbd$y), digits=4)
# 
# var(datalist.bbd$y)
# mtext(paste(c(mn.doy)))
# 
# hist(datalist.bbdcont$y, main = "Control plots")
# mn.doy.cont<-round(mean(datalist.bbdcont$y), digits=4)
# md.doy.cont<-round(median(datalist.bbdcont$y), digits=4)
# 
# mtext(paste(mn.doy.cont))
# 
# #Now plot doy~ temp for controls and all
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# plot(datalist.bbd$temp,datalist.bbd$y, main="All plots",xlab="temp",ylab="BB doy")
# abline(a=fixef(testm5.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
# plot(datalist.bbdcont$temp,datalist.bbdcont$y, main="Control plots",xlab="temp",ylab="BB doy")
# abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
# 
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# plot(datalist.bbd$mois,datalist.bbd$y, main="All plots",xlab="vwc",ylab="BB doy")
# abline(a=fixef(testm5.lmer)[1],b=fixef(testm5.lmer)[3], col="red")
# plot(datalist.bbdcont$mois,datalist.bbdcont$y, main="Control plots",xlab="vwc",ylab="BB doy")
# abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5.lmer)[3], col="red")
# 
# #Now plot doy~ temp for controls and all
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# plot(datalist.bbd$temp,datalist.bbd$y, main="All plots",xlab="temp",ylab="BB doy")
# abline(a=fixef(testm5.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
# plot(datalist.bbdcont$temp,datalist.bbdcont$y, main="Control plots",xlab="temp",ylab="BB doy")
# abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
# #look at correlation between mois and temp
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# plot(datalist.bbd$temp,datalist.bbd$mois, main="All plots, r=0.32",xlab="temp",ylab="vwc")
# abline(lm(datalist.bbd$mois~datalist.bbd$temp), col="red")
# cor(datalist.bbd$mois,datalist.bbd$temp)#0.32
# plot(datalist.bbdcont$temp,datalist.bbdcont$mois, main="Control plots, r=0.37",xlab="temp",ylab="vwc")
# abline(lm(datalist.bbdcont$mois~datalist.bbdcont$temp), col="red")
# cor(datalist.bbdcont$mois,datalist.bbdcont$temp)#0.37
# 

#try the model with brms
testm5.brms <- brm(y ~ temp * mois +#fixed effects
                     (temp * mois|sp) + (1|site/year), #random effects
                   data=datalist.bbd,
                   chains = 2, control = list(max_treedepth = 15,adapt_delta = 0.99))
# Code if you want to save your models (do NOT push output to git)
save(testm5.brms, file="Analyses/output/brms/testm5.brms.bb.Rda")

stancode(testm5.brms)
summary(testm5.brms)
quartz()
stanplot(testm5.brms, pars = "^b_")

stanplot(testm5.brms, surface = TRUE)
#22 divergent transitions
# #Fit model with control data only
# testm5cont.brms <- brm(y ~ temp * mois +#fixed effects
#                          (temp * mois|sp) + (1|site/year), #random effects
#                        data=datalist.bbdcont,
#                        chains = 2)# control = list(max_treedepth = 15,adapt_delta = 0.99)
# 
# 
# #try the model with brms
testm5cent.brms <- brm(y ~ temp * mois +#fixed effects
                          (temp * mois|sp) + (1|site/year), #random effects
                        data=datalist.bbd.cent,
                        chains = 2)#control = list(max_treedepth = 15,adapt_delta = 0.99)
save(testm5cent.brms, file="Analyses/output/brms/testm5cent.brms.bb.Rda")

testm5.rstan <- stan_lmer(formula = doy ~ ag_min_janmar * soilmois_janmar +#fixed effects
                                ((ag_min_janmar * soilmois_janmar)|genus.species) + (1|site/year), 
                              data = expgdd_bbd, chains=2)
save(testm5.rstan, file="Analyses/output/brms/testm5.rstanarm.bb.Rda")


#stanarm
testm5cent.rstan <- stan_lmer(formula = doy ~ ag_min_jm_cent * smjm_cent +#fixed effects
                        ((ag_min_jm_cent * smjm_cent)|genus.species) + (1|site/year), 
                         data = expgdd_bbd, chains=2)
summary(testm5.rstan)
coef(testm5cent.rstan)
head(testm5cent.rstan$stan_summary)
save(testm5cent.rstan, file="Analyses/output/brms/testm5cent.rstanarm.bb.Rda")

# # without control, had divergent transisions and #2 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. but took a really long time to fit...7692.48 seconds (=2.1368 hrs per chain)

#Fit model without soil moisture
#stanarm
testm5centnosm.rstan <- stan_lmer(formula = doy ~ ag_min_jm_cent[,1] +#fixed effects
                                (ag_min_jm_cent[,1] |genus.species) + (1|site/year), 
                              data = expgdd_bbd, chains=2)
summary(testm5centnosm.rstan)
coef(testm5centnosm.rstan)
head(testm5centnosm.rstan$stan_summary)
save(testm5centnosm.rstan, file="Analyses/output/brms/testm5nosm.rstanarm.bb.Rda")


# stancode(testm5cent.brms)#took 15986.5 seconds for one chain, 15185.4 for the other (~4 hours per chain)
# summary(testm5cent.brms)
# stanplot(testm5cent.brms, pars = "^b_", title="Budburst model, with species and site/year random effects")
# #a: 99.02, temp=--10.40, mois=-1.37, tmint=0.29

#make plots of main effects and species- level effects of this model 
mod<-testm5.rstan
quartz()
species=as.numeric(rownames(coef(mod)$genus.species[,,2]))
plot(coef(mod)$genus.species[,1,2],1:length(species),type="p",pch=21,bg="darkred",xlab="Temperature effect (days)", ylab=" ", yaxt="n",cex=1.2, xlim=c(-100,20), ylim=c(0,100))
#coef(testm5cent.brms)$sp[,1,2]
abline(v=0)
for (i in 1:length(coef(mod)$genus.species[,1,2])){
  arrows(coef(mod)$genus.species[i,3,2],i,coef(mod)$genus.species[i,4,2],i, code=0)
}
for (i in 1:length(coef(mod)$sgenus.species[,1,3])){
  arrows(coef(mod)$sp[i,3,3],i,coef(mod)$sgenus.species[i,4,3],i, code=0)
}
points(coef(testm5cent.brms)$sp[,1,2],1:length(species),pch=21,bg="darkred")
points(coef(testm5cent.brms)$sp[,1,3],1:length(species),pch=21,bg="darkblue")

#fixed effects
coefs<-c(95,85,75,65)
for (i in 1:length(fixef(testm5cent.brms)[,1])){
  arrows(fixef(testm5cent.brms)[i,3],coefs[i],fixef(testm5cent.brms)[i,4],coefs[i], code=0)
}
points(fixef(testm5cent.brms)[,1],coefs,pch=21,bg=c("gray","darkred","darkblue","purple4"), cex=2)
axis(2,at=c(95,85,75,65,50),labels=c("intercept","temp","mois","temp*mois","species"), las=2)

load("Analyses/output/brms/testm5.brms.bb.Rda")


#use lizzies code now
mod<-testm5.brms
sum<-summary(mod)
fix<-sum$fixed
speff <- coef(mod)
rownames(fix)<-c("Intercept","Temperature","Moisture","Temp*Mois")
#pdf(file.path("Analyses/soilmoisture/figures/m5.bbd.pdf"), width = 8, height = 6)
quartz(width = 7, height = 7)
par(mfrow=c(3,1), mar = c(4, 7, .5, 1))
# One panel: budburst
plot(seq(-35, #min(meanz[,'mean']*1.1),
         300, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix)+1, length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of budburst",
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])
for(i in 1:nrow(fix)){
  arrows(speff$sp[,"97.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"2.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha("darkgray", 0.2)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i], 5*(nrow(fix):1)[i]-.5-sp,
         pch = 16,
         col = alpha("darkgray", 0.5))
}
#fixed effects
arrows(fix[,"u-95% CI"], 5*(nrow(fix):1), fix[,"l-95% CI"], 5*(nrow(fix):1),
       len = 0, col = "black", lwd = 2)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 1.2,
       col = "springgreen")

abline(v = 0, lty = 2)
#dev.off()




#LUD and LOD in lmer
mean(expgdd_bbd$doy, na.rm=TRUE)#97.72388
mean(expgdd_lod$doy, na.rm=TRUE)#143.1189= april/may
datalist.lod<- with(expgdd_lod, 
                    list(y = doy, 
                         temp = ag_min_aprjun, #above-ground minimum air temp
                         mois = soilmois_aprjun, #soil moisture
                         sp = genus.species,
                         site = site,
                         year = year,
                         N = nrow(expgdd_bbd),
                         n_sp = length(unique(expgdd_bbd$genus.species))
                    )
)


datalist.lod.cent <- with(expgdd_lod, 
                          list(y = doy, 
                               temp = ag_min_aprjun_cent, #above-ground minimum air temp
                               mois = soilmois_aprjun_cent, #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_bbd),
                               n_sp = length(unique(expgdd_bbd$genus.species))
                          )
)


lod.testm5.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site/year),data=datalist.lod)#
summary(lod.testm5.lmer)#failed to converge with uncentered data! 
#temp= -3.8227; mois=-71.7425; temp:mois= 5.1062


lodcent.testm5.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site/year),data=datalist.lod.cent)#
summary(lodcent.testm5.lmer)
#temp= -9.9582; mois=--0.6598; temp:mois= 0.6661
testm5.lod.brms <- brm(y ~ temp * mois +#fixed effects
                             (temp * mois|sp) + (1|site/year), #random effects
                           data=datalist.lod,
                           chains = 2,control = list(max_treedepth = 15,adapt_delta = 0.99))
save(testm5.lod.brms, file="Analyses/output/brms/testm5.brms.lo.Rda")

testm5cent.lod.brms <- brm(y ~ temp * mois +#fixed effects
                             (temp * mois|sp) + (1|site/year), #random effects
                           data=datalist.lod.cent,
                           chains = 2,control = list(max_treedepth = 15,adapt_delta = 0.99))


testm5cent.lod.rstan <- stan_lmer(formula = doy ~ ag_min_aprjun_cent * soilmois_aprjun_cent +#fixed effects
                                (ag_min_aprjun_cent * soilmois_aprjun_cent|genus.species) + (1|site/year), 
                              data = expgdd_lod, chains=2)

summary(testm5cent.lod.rstan)
coef(testm5cent.lod.rstan)
head(testm5cent.rstan$stan_summary)

save(testm5cent.lod.rstan, file="Analyses/output/brms/testm5.rstanarm.lo.Rda")

mod<-testm5cent.lod.rstan
sum<-summary(mod)
speff <- coefficients(mod)$genus.species
#colnames(speff)<-c("Intercept","Temperature","Moisture","Temp*Mois")

#pdf(file.path("Analyses/soilmoisture/figures/m5.bbd.pdf"), width = 8, height = 6)
#quartz(width = 8, height = 6)
#par(mfrow=c(1,1), mar = c(6, 10, 2, 1))
# One panel: budburst
plot(seq(-30, 
         300, 
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix)+1, length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of leafout",
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])
for(i in 1:nrow(fix)){
  arrows(speff$sp[,"97.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"2.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha("darkgray", 0.2)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i], 5*(nrow(fix):1)[i]-.5-sp,
         pch = 16,
         col = alpha("darkgray", 0.5))
}
#fixed effects
arrows(fix[,"u-95% CI"], 5*(nrow(fix):1), fix[,"l-95% CI"], 5*(nrow(fix):1),
       len = 0, col = "black", lwd = 2)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 1.2,
       col = "darkgreen")

abline(v = 0, lty = 2)
#dev.off()

#datalist.lud.cent <- with(expgdd_lud, 
#                         list(y = doy, 
#                             temp = ag_min_jm_cent, #above-ground minimum air temp
#                            mois = smjm_cent, #soil moisture
#                           sp = genus.species,
#                          site = site,
#                         year = year,
#                        N = nrow(expgdd_bbd),
#                       n_sp = length(unique(expgdd_bbd$genus.species))
#                 )
#)

#datalist.lud <- with(expgdd_lud, 
#                          list(y = doy, 
#                               temp = ag_min_janmar, #above-ground minimum air temp
#                               mois = soilmois_janmar, #soil moisture
#                               sp = genus.species,
#                               site = site,
#                               year = year,
#                               N = nrow(expgdd_bbd),
#                               n_sp = length(unique(expgdd_bbd$genus.species))
#                          )
#)

#lud.testm5.lmer<-lmer(y~temp * mois +
#                        (temp*mois|sp)+ (1|site/year),
#                      data=datalist.lud)#
#summary(lud.testm5.lmer)#model fits! 
#temp= -7.893; mois=7.655; temp:mois= 18.238
#ludcent.testm5.lmer<-lmer(y~temp * mois +
#                        (temp*mois|sp)+ (1|site/year),
#                      data=datalist.lud.cent)#converged when site variance =3
#summary(ludcent.testm5.lmer)#model fits! 
#temp= -14.0005; mois=0.9292; temp:mois= 2.2634

datalist.ffd.cent <- with(expgdd_ffd, 
                          list(y = doy, 
                               temp = agtmin_cent, #above-ground minimum air temp
                               mois = sm_cent, #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_bbd),
                               n_sp = length(unique(expgdd_bbd$genus.species))
                          )
)

testm5cent.ffd.brms <- brm(y ~ temp * mois +#fixed effects
                             (temp * mois|sp) + (1|site/year), #random effects
                           data=datalist.ffd.cent,
                           chains = 2,control = list(max_treedepth = 15,adapt_delta = .999))


mod<-testm5cent.ffd.brms
sum<-summary(mod)
fix<-sum$fixed
speff <- coef(mod)
rownames(fix)<-c("Intercept","Temperature","Moisture","Temp*Mois")

#pdf(file.path("Analyses/soilmoisture/figures/m5.bbd.pdf"), width = 8, height = 6)
#quartz(width = 8, height = 6)
#par(mfrow=c(1,1), mar = c(6, 10, 2, 1))
# One panel: budburst
plot(seq(-25, #min(meanz[,'mean']*1.1),
         300, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix)+1, length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of flowering",
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])
for(i in 1:nrow(fix)){
  arrows(speff$sp[,"97.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"2.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha("darkgray", 0.2)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i], 5*(nrow(fix):1)[i]-.5-sp,
         pch = 16,
         col = alpha("darkgray", 0.5))
}
#fixed effects
arrows(fix[,"u-95% CI"], 5*(nrow(fix):1), fix[,"l-95% CI"], 5*(nrow(fix):1),
       len = 0, col = "black", lwd = 2)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 1.2,
       col = "purple3")

abline(v = 0, lty = 2)
#dev.off()




###For supplement
datalist.ffrd.cent <- with(expgdd_ffrd, 
                           list(y = doy, 
                                temp = agtmin_cent, #above-ground minimum air temp
                                mois = sm_cent, #soil moisture
                                sp = genus.species,
                                site = site,
                                year = year,
                                N = nrow(expgdd_bbd),
                                n_sp = length(unique(expgdd_bbd$genus.species))
                           )
)

testm5cent.ffrd.brms <- brm(y ~ temp * mois +#fixed effects
                              (temp * mois|sp) + (1|site/year), #random effects
                            data=datalist.ffrd.cent,
                            chains = 2,control = list(max_treedepth = 15,adapt_delta = .999))

datalist.sen.cent <- with(expgdd_sen, 
                          list(y = doy, 
                               temp = agtmin_cent, #above-ground minimum air temp
                               mois = sm_cent, #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_bbd),
                               n_sp = length(unique(expgdd_bbd$genus.species))
                          )
)

testm5cent.sen.brms <- brm(y ~ temp * mois +#fixed effects
                             (temp * mois|sp) + (1|site/year), #random effects
                           data=datalist.sen.cent,
                           chains = 2,control = list(max_treedepth = 15,adapt_delta = .999))


ffd.testm5.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site/year),data=datalist.ffd.cent)#converged when site variance =3
summary(ffd.testm5.lmer)#model fits! 
#temp= -7.1193; mois=-2.4218 ; temp:mois= -2.4283


ffrd.testm5.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site/year),data=datalist.ffrd.cent)#converged when site variance =3
summary(ffrd.testm5.lmer)#model fits! 
#temp= -5.1190; mois=-0.3049 ; temp:mois= -0.7659 

sen.testm5.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site/year),data=datalist.sen.cent)#converged when site variance =3
summary(sen.testm5.lmer)#model fits! 
#temp= 1.311 ; mois=-8.317 ; temp:mois= -4.695

##Figure of FFRD and SEN
mod<-testm5cent.ffrd.brms 
sum<-summary(mod)
fix<-sum$fixed
speff <- coef(mod)
rownames(fix)<-c("Intercept","Temperature","Moisture","Temp*Mois")
#pdf(file.path("Analyses/soilmoisture/figures/m5.bbd.pdf"), width = 8, height = 6)
quartz(width = 8, height = 7)
par(mfrow=c(2,1), mar = c(4, 7, .5, 1))
# One panel: fruiting
plot(seq(-45, #min(meanz[,'mean']*1.1),
         360, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix)+1, length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of fruiting",
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])
for(i in 1:nrow(fix)){
  arrows(speff$sp[,"97.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"2.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha("darkgray", 0.2)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i], 5*(nrow(fix):1)[i]-.5-sp,
         pch = 16,
         col = alpha("darkgray", 0.5))
}
#fixed effects
arrows(fix[,"u-95% CI"], 5*(nrow(fix):1), fix[,"l-95% CI"], 5*(nrow(fix):1),
       len = 0, col = "black", lwd = 2)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 1.2,
       col = "midnight blue")

abline(v = 0, lty = 2)
#dev.off()

mod<-testm5cent.sen.brms 
sum<-summary(mod)
fix<-sum$fixed
speff <- coef(mod)
rownames(fix)<-c("Intercept","Temperature","Moisture","Temp*Mois")
#pdf(file.path("Analyses/soilmoisture/figures/m5.sen.pdf"), width = 8, height = 6)

# One panel: senescence
plot(seq(-45, #min(meanz[,'mean']*1.1),
         360, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix)+1, length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of senescence",
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])
for(i in 1:nrow(fix)){
  arrows(speff$sp[,"97.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"2.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha("darkgray", 0.2)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i], 5*(nrow(fix):1)[i]-.5-sp,
         pch = 16,
         col = alpha("darkgray", 0.5))
}
#fixed effects
arrows(fix[,"u-95% CI"], 5*(nrow(fix):1), fix[,"l-95% CI"], 5*(nrow(fix):1),
       len = 0, col = "black", lwd = 2)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 1.2,
       col = "burlywood4")

abline(v = 0, lty = 2)
#dev.off()

#Now explore the implications of this model a bit:
#To do:
#1) Does soil moisture have a stronger effect in experimental than non experimental data?
#Why might it? possibly because experiments are often with smaller/younger stages/less developed root systems whereas observational studies are 
#more likely to be of adult individuals with well-developed roots.
#Are individuals in the studies in our data set small? large? young/old?
unique(expgdd_subs$site2[expgdd_subs$event=="bbd"])#force
#"exp01" "exp03" "exp04" "exp07" "exp09"
unique(expgdd_subs$site2[expgdd_subs$event=="lod"])
#"exp01" "exp03" "exp04" "exp07" "exp10"

#What stage/age/size are plants in these experiments?
bbdset<-expgdd_subs[expgdd_subs$event=="bbd",]
# exp01- BACE-has a mix of established plants (forbs and grasses) 
# and planted tree seedlings (Acer rubrum, Betula lenta, Pinus strobus, Quercus rubra)
# exp03: tree seeds sowed
# exp04: tree seeds sowed
# exp07: pre-existing vegetation
# exp10: pre-existing vegetation
table(bbdset$site2,bbdset$styear)
table(expgdd_subs$sp.name[expgdd_subs$site2 =="exp01"],expgdd_subs$year[expgdd_subs$site2 =="exp01"])
#does effect of soil moisture vary by veg type (planted vs pre-existing?)
head(expgdd_bbd)
expgdd_bbd$vegtype<-"existing"
expgdd_bbd$vegtype[expgdd_bbd$site2=="exp03"|expgdd_bbd$site2=="exp04"]<-"planted"
expgdd_bbd$vegtype[which(expgdd_bbd$site2=="exp01" & expgdd_bbd$sp.name=="Acer.rubrum")]<-"planted"
expgdd_bbd$vegtype[which(expgdd_bbd$site2=="exp01" & expgdd_bbd$sp.name=="Betula.lenta")]<-"planted"
expgdd_bbd$vegtype[which(expgdd_bbd$site2=="exp01" & expgdd_bbd$sp.name=="Quercus.rubra")]<-"planted"
expgdd_bbd$vegtype[which(expgdd_bbd$site2=="exp01" & expgdd_bbd$sp.name=="Pinus.strobus")]<-"planted"

unique(expgdd_bbd$vegtype[which(expgdd_bbd$site2=="exp01")])
expgdd_bbd$vegtype<-as.factor(expgdd_bbd$vegtype)
datalist.bbd <- with(expgdd_bbd, 
                     list(y = doy, 
                          temp = ag_min_janmar, #above-ground minimum air temp
                          mois = soilmois_janmar, #soil moisture
                          sp = genus.species,
                          site = site,
                          year = styear,
                          vegtype=vegtype,
                          N = nrow(expgdd_bbd),
                          n_sp = length(unique(expgdd_bbd$genus.species))
                     )
)
testm6.lmer.2wy.nosp<-lmer(y~((temp + mois + vegtype)^2) 
                  + (1|site/year),
                  data=datalist.bbd)
testm6.lmer.nosp<-lmer(y~(temp * mois * vegtype) + 
                         (1|site/year),
                           data=datalist.bbd)

testm6.lmer<-lmer(y~(temp * mois * vegtype) #fixed effects 
                  + ((temp * mois)|sp) + (1|site/year), #random effects
                   data=datalist.bbd)
AIC(testm6.lmer.2wy.nosp,testm6.lmer.nosp,testm6.lmer,testm5.lmer)
#does effect of soil moisture diminish with time (for bace)?

#