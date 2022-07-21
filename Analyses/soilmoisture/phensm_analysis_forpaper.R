#Started Sept 2017
#By Ailene
#Questions addressed with the models below:
#1) How do soil moisture and temperature affect doy of bud burst, leaf out, flowering, fruiting, and senescence?
#2) How does GDD at the time of an event vary with soil moisture? Do effects of soil moisture differ for bb, lo, and fl?
#Approach: Use brms/Stan to fit soil moisture- phenology model to radcliffe data 

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

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/radcliffe")}

#setwd("~/GitHub/radcliffe")#tnc

#Goal: Fit a multi-model to phenology (budburst) data with temperature, soil moisture, and 
#their interaction as explanatory variables.

source('Analyses/soilmoisture/savestan.R')

#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)

remove.conifers=TRUE
use.airtemponly=TRUE
use.centmod=FALSE


#standard data wrangling to get expclim2 for climate analyses and expgdd for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")
#54352    51

#summarize climate data by plot (annual and seasonal temp, soil mois), 
#merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs

if(use.airtemponly==TRUE) {source("Analyses/source/climsum_byplot.R")}
if(use.airtemponly==FALSE) {source("Analyses/source/climsum_byplot_soiltoo.R")}

##################################
#####Fit DOY Phenology Models#####
##################################

#Check: Look at which studies have both SM and AG temp data
#which(tapply(expclim2$agtemp_mn,expclim2$site,mean,na.rm=T)>0)
#which(tapply(expclim2$soilmois1,expclim2$site,mean,na.rm=T)>0)

#Prep the data for models and divide into datasets by phenophase
source("Analyses/source/stanprep_phenmods.R")

##################
# Fit m5 to data #
##################


#try the model with brms
testm5cent.brms <- brm(y ~ temp * mois +#fixed effects
                     (temp * mois|sp) + (1|site/year), #random effects
                   data=datalist.bbd.cent,
                   chains = 2,iter = 3000,
                   control = list(max_treedepth = 15,adapt_delta = 0.999))

# stancode(testm5cent.brms)#took 15986.5 seconds for one chain, 15185.4 for the other (~4 hours per chain)
# summary(testm5cent.brms)
# stanplot(testm5cent.brms, pars = "^b_", title="Budburst model, with species and site/year random effects")
# #a: 99.02, temp=--10.40, mois=-1.37, tmint=0.29
save(testm5cent.brms, file="Analyses/output/brms/testm5cent.brms.bb.Rda")
round(fixef(testm5cent.brms, probs=c(.90,0.10)), digits=2)

#now fit with data containing same species as lo
#try the model with brms
testm5cent.brms <- brm(y ~ temp * mois +#fixed effects
                         (temp * mois|sp) + (1|site/year), #random effects
                       data=datalist.bbdlo.cent,
                       chains = 1,iter = 4000,
                       control = list(max_treedepth = 15,adapt_delta = 0.999))
# 
# 

save(testm5cent.brms, file="Analyses/output/brms/testm5cent.brms.bblo.Rda")

#make plots of main effects and species- level effects of this model
#Pull out species with strongest effects of moisture
# spofint<-c(30,104,98,134,182,25,24,21,1,23,139)
# spofit<-sort(spofint)
# spofint2<-unique(expgdd_bbd$sp.name[expgdd_bbd$genus.species==spofint])
# spofint2<-c("Carya.glabra",spofint2)
# spofit2<-sort(spofint2)
# ranef(mod)$sp[30,,]
# ranef(mod)$sp[rownames(ranef(mod)$sp)=="30",,]
# #"Carya.glabra"#30 (-4.341) moisture effect, temo effect=-11.9
# #at "exp03" "exp04" Duke and HF
# 
# #104 (-6.04) "exp03" "exp04" Duke and HF
# ranef(mod)$sp[rownames(ranef(mod)$sp)=="104",,]#"Magnolia.grandiflora"
# 

testm5cent.lod.brms <- brm(y ~ temp * mois +#fixed effects
                         (temp * mois|sp) + (1|site/year), #random effects
                       data=datalist.lod.cent,
                       chains = 2,iter = 4000,
                       control = list(max_treedepth = 15,adapt_delta = 0.99))
save(testm5cent.lod.brms, file="Analyses/output/brms/testm5cent.brms.lo.Rda")
round(fixef(testm5cent.lod.brms, probs=c(.90,0.10)), digits=2)

#now fit with data containing same species as lo
#try the model with brms
testm5cent.lodbb.brms <- brm(y ~ temp * mois +#fixed effects
                         (temp * mois|sp) + (1|site/year), #random effects
                       data=datalist.lodbb.cent,
                       chains = 2,iter = 4000,
                       control = list(max_treedepth = 15,adapt_delta = 0.99))

# without control, had divergent transisions and #2 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. but took a really long time to fit...7692.48 seconds (=2.1368 hrs per chain)
# 
# 
# stancode(testm5cent.lodbb.brms)#took 15986.5 seconds for one chain, 15185.4 for the other (~4 hours per chain)
# summary(testm5cent.lodbb.brms)
# stanplot(testm5cent.lodbb.brms, pars = "^b_", title="Budburst model, with species and site/year random effects")
# #a: 99.02, temp=--10.40, mois=-1.37, tmint=0.29
save(testm5cent.lodbb.brms, file="Analyses/output/brms/testm5cent.brms.lobb.Rda")


testm5cent.lodfl.brms <- brm(y ~ temp * mois +#fixed effects
                               (temp * mois|sp) + (1|site/year), #random effects
                             data=datalist.lodfl.cent,
                             chains = 2,iter = 4000,
                             control = list(max_treedepth = 15,adapt_delta = 0.9999))
save(testm5cent.lodfl.brms, file="Analyses/output/brms/testm5cent.brms.lofl.Rda")
#1 divergent transition of warmup with 

testm5cent.ffd.brms <- brm(y ~ temp * mois +#fixed effects
                             (temp * mois|sp) + (1|site/year), #random effects
                           data=datalist.ffd,
                           chains = 2,iter = 6000,
                           control = list(max_treedepth = 15,adapt_delta = .999))

save(testm5cent.ffd.brms, file="Analyses/output/brms/testm5cent.brms.ff.Rda")

round(fixef(testm5cent.ffd.brms, probs=c(.90,0.10)), digits=2)

testm5cent.ffdlo.brms <- brm(y ~ temp * mois +#fixed effects
                             (temp * mois|sp) + (1|site/year), #random effects
                           data=datalist.ffdlo.cent,
                           chains = 2,iter = 4000,control = list(max_treedepth = 15,adapt_delta = .9999))

save(testm5cent.ffdlo.brms, file="Analyses/output/brms/testm5cent.brms.fflo.Rda")

testm5cent.ffrd.brms <- brm(y ~ temp * mois +#fixed effects
                             (temp * mois|sp) + (1|site/year), #random effects
                           data=datalist.ffrd.cent,
                           chains = 2,iter = 5000,
                           control = list(max_treedepth = 15,adapt_delta = .9999))
save(testm5cent.ffrd.brms, file="Analyses/output/brms/testm5cent.brms.frd.Rda")
round(fixef(testm5cent.ffrd.brms, probs=c(.90,0.10)), digits=2)


testm5cent.sen.brms <- brm(y ~ temp * mois +#fixed effects
                              (temp * mois|sp) + (1|site/year), #random effects
                            data=datalist.sen.cent,
                            chains = 2,iter = 6000,
                           control = list(max_treedepth = 15,adapt_delta = .9999))

save(testm5cent.sen.brms, file="Analyses/output/brms/testm5cent.brms.sen.Rda")
summary(testm5cent.sen.brms)


#table with sites, phenophases
phentab<-table(exppheno$site,exppheno$event)

unique(datalist.bbd.cent$site)# "exp01" "exp03" "exp04" "exp07" "exp10",1 3 4 5 7

#Are individuals in the studies in our data set small? large? young/old?
unique(expgdd_subs$site2[expgdd_subs$event=="bbd"])
unique(expgdd_subs$site2[expgdd_subs$event=="lod"])
#how many species for exp07 and exp09
length(unique(expgdd_subs$sp.name[expgdd_subs$site2=="exp07"]))

#"exp01" "exp03" "exp04" "exp07" "exp10"
#What stage/age/size are plants in these experiments?
bbdset<-expgdd_subs[expgdd_subs$event=="bbd",]
# exp01- has a mix of established plants (forbs and grasses) 
# and planted tree seedlings (Acer rubrum, Betula lenta, Pinus strobus, Quercus rubra)
# exp03: tree seeds sowed
# exp04: tree seeds sowed
# exp07: pre-existing vegetation
# exp10: pre-existing vegetation
table(bbdset$site2,bbdset$styear)
table(expgdd_subs$sp.name[expgdd_subs$site2 =="exp01"],expgdd_subs$year[expgdd_subs$site2 =="exp01"])

#does effect of soil moisture diminish with time (for bace)?

#uncentered bb mod
testm5.brms <- brm(y ~ temp * mois +#fixed effects
                         (temp * mois|sp) + (1|site/year), #random effects
                       data=datalist.bbd,
                       chains = 2,iter = 4000,
                       control = list(max_treedepth = 15,adapt_delta = 0.99))

# stancode(testm5.brms)#took 15986.5 seconds for one chain, 15185.4 for the other (~4 hours per chain)
# summary(testm5.brms)
# stanplot(testm5cent.brms, pars = "^b_", title="Budburst model, with species and site/year random effects")
# #a: 99.02, temp=--10.40, mois=-1.37, tmint=0.29
save(testm5.brms, file="Analyses/output/brms/testm5.brms.bb.Rda")
round(fixef(testm5.brms, probs=c(.90,0.10)), digits=2)

#uncentered lo mod
testm5.brms <- brm(y ~ temp * mois +#fixed effects
                     (temp * mois|sp) + (1|site/year), #random effects
                   data=datalist.lod,
                   chains = 2,iter = 4000,
                   control = list(max_treedepth = 15,adapt_delta = 0.99))
save(testm5.brms, file="Analyses/output/brms/testm5.brms.lo.Rda")
round(fixef(testm5.brms, probs=c(.90,0.10)), digits=2)

# stancode(testm5.brms)#took 15986.5 seconds for one chain, 15185.4 for the other (~4 hours per chain)
# summary(testm5.brms)
# stanplot(testm5cent.brms, pars = "^b_", title="Budburst model, with species and site/year random effects")
# #a: 99.02, temp=--10.40, mois=-1.37, tmint=0.29
#uncentered lo mod
testm5.brms <- brm(y ~ temp * mois +#fixed effects
                     (temp * mois|sp) + (1|site/year), #random effects
                   data=datalist.ffd,
                   chains = 2,iter = 4000,
                   control = list(max_treedepth = 15,adapt_delta = 0.99))
save(testm5.brms, file="Analyses/output/brms/testm5.brms.lo.Rda")
round(fixef(testm5.brms, probs=c(.90,0.10)), digits=2)

#
save(testm5.brms, file="Analyses/output/brms/testm5.brms.bb.Rda")
round(fixef(testm5.brms, probs=c(.90,0.10)), digits=2)

#############################
####     GDD models     #####
#############################

#Budburst
expgdd_bb<-expgdd_gdd[expgdd_gdd$event=="bbd",]
expgdd_bb$site<-as.factor(expgdd_bb$site)
expgdd_bb$year<-as.factor(expgdd_bb$year)

gddmbb.brms <- brm(cumgdd_air ~ soilmois_janmar +#fixed effects
                               (soilmois_janmar|genus.species) + (1|site/year), #random effects
                             data=expgdd_bb,
                             chains = 2,iter = 2000,control = list(max_treedepth = 15,adapt_delta = 0.99))
save(gddmbb.brms, file="Analyses/output/brms/gddmbb.Rda")
round(fixef(gddmbb.brms, probs=c(.90,0.10)), digits=2)
summary(gddmbb.brms)
gddmbb2.brms <- brm(cumgdd_air ~ soilmois_janmar +#fixed effects
                     (1|genus.species) + (1|site/year), #random effects
                   data=expgdd_bb,
                   chains = 2,iter = 2000,control = list(max_treedepth = 15,adapt_delta = 0.99))
save(gddmbb2.brms, file="Analyses/output/brms/gddmbb_ranint.Rda")
round(fixef(gddmbb2.brms, probs=c(.90,0.10)), digits=2)
summary(gddmbb2.brms)
plot(gddmbb2.brms)
conditional_effects(gddmbb2.brms)

## GDD lmer models for quicker run/comparison
# gddmbb <- lmer(cumgdd_air ~ soilmois_janmar +#fixed effects
#                      (soilmois_janmar|genus.species) + (1|site/year), #random effects
#                    data=expgdd_bb)#explains more variation, but error around main effect of soil mois is larger than ran intercept mod...not sure about this
# gddmbb2 <- lmer(cumgdd_air ~ soilmois_janmar +#fixed effects
#                  (1|genus.species) + (1|site/year), #random effects
#                data=expgdd_bb)
# 
# gddmbbnull <- lmer(cumgdd_air ~ 1 +#fixed effects
#                  (1|genus.species) + (1|site/year), #random effects
#                data=expgdd_bb)
# AIC(gddmbb,gddmbb2,gddmbbnull)
# summary(gddmbb)


#Leafout
expgdd_lo<-expgdd_gdd[expgdd_gdd$event=="lod",]
gddmlo.brms <- brm(cumgdd_air ~ soilmois_aprjun +#fixed effects
                     (soilmois_aprjun|genus.species) + (1|site/year), #random effects
                   data=expgdd_lo,
                   chains = 2,iter = 2000,
                   control = list(max_treedepth = 15,adapt_delta = 0.99))
save(gddmlo.brms, file="Analyses/output/brms/gddmlo_ranslope.Rda")
round(fixef(gddmlo.brms, probs=c(.90,0.10)), digits=2)
summary(gddmlo.brms)
gddmlo2.brms <- brm(cumgdd_air ~ soilmois_aprjun +#fixed effects
                      (1|genus.species) + (1|site/year), #random effects
                    data=expgdd_lo,
                    chains = 2,iter = 2000,control = list(max_treedepth = 15,adapt_delta = 0.99))
save(gddmlo2.brms, file="Analyses/output/brms/gddmlo_ranint.Rda")
round(fixef(gddmlo2.brms, probs=c(.90,0.10)), digits=2)
summary(gddmlo2.brms)
plot(gddmlo2.brms)
conditional_effects(gddmlo2.brms)
# # GDD lmer models for quicker run/comparison
# gddmlo <- lmer(cumgdd_air ~ soilmois_aprjun +#fixed effects
#                  (soilmois_aprjun|genus.species) + (1|site/year), #random effects
#                data=expgdd_lo)
# gddmlo2 <- lmer(cumgdd_air ~ soilmois_aprjun +#fixed effects
#                  (1|genus.species) + (1|site/year), #random effects
#                data=expgdd_lo)
# gddmlonull <- lmer(cumgdd_air ~ 1 +#fixed effects
#                      (1|genus.species) + (1|site/year), #random effects
#                    data=expgdd_lo)
# AIC(gddmlo,gddmlo2,gddmlonull)
# summary(gddmlo)

#Flowering

expgdd_fl<-expgdd_gdd[expgdd_gdd$event=="ffd",]
# gddmfl.brms <- brm(cumgdd_air ~ soilmois_aprjun +#fixed effects
#                      (soilmois_aprjun|genus.species) + (1|site/year), #random effects
#                    data=expgdd_fl,
#                    chains = 2,iter = 4000,
#                    control = list(max_treedepth = 15,adapt_delta = 0.99))
## random slopes model not possible for fl, 
gddmfl2.brms <- brm(cumgdd_air ~ soilmois_aprjun +#fixed effects
                     (1|genus.species) + (1|site), #random effects
                   data=expgdd_fl,
                   chains = 2,iter = 4000,
                   control = list(max_treedepth = 15,adapt_delta = 0.99))
#get 2-3 divergent transitions, wide error
save(gddmfl2.brms, file="Analyses/output/brms/gddmfl_ranint.Rda")
round(fixef(gddmfl2.brms, probs=c(.90,0.10)), digits=2)
summary(gddmfl2.brms)
plot(gddmfl2.brms)
conditional_effects(gddmfl2.brms)
# # GDD lmer models for quicker run/comparison

# GDD lmer models for quicker run/comparison
# gddmfl <- lmer(cumgdd_air ~ soilmois_janmar +#fixed effects
#                  (soilmois_janmar|genus.species) + (1|site), #random effects
#                data=expgdd_fl)
# gddmfl3 <- lmer(cumgdd_air ~ soilmois_aprjun +#fixed effects
#                  (soilmois_aprjun|genus.species) + (1|site), #random effects
#                data=expgdd_fl)
# gddmflnull <- lmer(cumgdd_air ~ 1 +#fixed effects
#                      (1|genus.species) + (1|site), #random effects
#                    data=expgdd_fl)
# AIC(gddmfl,gddmfl3)
# summary(gddmfl)

#Some basic plots
plot(expgdd_bb$soilmois_janmar,expgdd_bb$cumgdd_air,col=as.factor(expgdd_bb$site))
plot(expgdd_lo$soilmois_aprjun,expgdd_lo$cumgdd_air,col=as.factor(expgdd_lo$site))

####################################################################################
##########OLD CODE: TO DELETE####################################################
#################################################################################
# #Old stan model code
# #################################################################
# # m1: a(sp) + t(sp) + m(sp) + t*m(sp) (ran eff only of species) #
# #################################################################
# m1 = stan('Analyses/soilmoisture/M1_bbd.stan', data = datalist.bbd,
#                iter = 2500, warmup=1500) # 
#  
# save(m1, file="Analyses/soilmoisture/M1_bbd.rda")
# #load("Analyses/soilmoisture/M1_bbd.Rda")
# m1.sum <- summary(m1)$summary 
# #head(m1.sum) 
# m1.sum[grep("mu_a_sp", rownames(m1.sum)),]
# m1.sum[grep("b_temp", rownames(m1.sum)),]
# m1.sum[grep("b_mois", rownames(m1.sum)),]
# m1.sum[grep("b_tm", rownames(m1.sum)),]
# launch_shinystan(m1)
# #################################################################
# # m2: a(sp) +a(site)+ t(sp) + m(sp) + t*m(sp) (crossed ran eff of species and site) #
# #################################################################
# #2) Make a list out of the processed data. It will be input for the model.
# ####The below model is not ready yet!!!!
# datalist2.bbd <- with(expgdd_bbd, 
#                      list(y = doy, 
#                           temp = ag_min_janmar, #above-ground minimum air temp
#                           mois = soilmois_janmar, #soil moisture
#                           sp = genus.species,
#                           site = site,
#                           N = nrow(expgdd_bbd),
#                           n_sp = length(unique(expgdd_bbd$genus.species)),
#                           n_site = length(unique(expgdd_bbd$site))
#                      )
# )
# m2 = stan('Analyses/soilmoisture/M2_bbd.stan', data = datalist.bbd,
#           iter = 2500, warmup=1500) # 
# 
# save(m2, file="Analyses/soilmoisture/M2_bbd.rda")
# #load("Analyses/soilmoisture/M2_bbd.Rda")
# m2.sum <- summary(m1)$summary 
# head(m2.sum) 
# m2.sum[grep("mu_a_sp", rownames(m2.sum)),]
# m2.sum[grep("b_temp", rownames(m2.sum)),]
# m2.sum[grep("b_mois", rownames(m2.sum)),]
# m2.sum[grep("b_tm", rownames(m2.sum)),]
# 
# 
# #data=list(y=y,sp=sp,site=site,temp=temp, mois=mois,n_sp=n_sp,n_site=n_site,N=N)) 
# datalist.bbd <- with(expgdd_bbd, 
#                       list(y = doy, 
#                            sp = genus.species,
#                            site = site,
#                            temp = temp = ag_min_jm_cent[,1], #above-ground minimum air temp
#                            mois = smjm_cent[,1], #soil moisture as a proportion
#                            n_sp = length(unique(expgdd_bbd$genus.species)),
#                            n_site = length(unique(expgdd_bbd$site)),
#                            N = nrow(expgdd_bbd)
#                            )
#                       )
# 
# m4 = stan('Analyses/soilmoisture/M4_bbd_testdata.stan', data = datalist.bbd,
#           iter = 2500, warmup=1500) # 
# 
# 
# 
# 
# #1) How do warming and precip treatments affect temperature and soil moisture? (Make plots and fit models)
# 
# expclim2$target_cent<-scale(expclim2$target, center = TRUE, scale = TRUE)
# expclim2$preciptreat_amt_cent<-scale(expclim2$preciptreat_amt, center = TRUE, scale = TRUE)
# expclim2a<-subset(expclim2,select=c(site,year,doy,target_cent,preciptreat_amt,target,preciptreat_amt_cent,soilmois1,agtemp_mean))
# expclim2a<- expclim2a [apply(expclim2a , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
# expclim2a$doy<-as.factor(expclim2a$doy)
# expclim2a$year<-as.factor(expclim2a$year)
# expclim2a$site<-as.factor(expclim2a$site)
# expclim2a$preciptreat_prop<-expclim2a$preciptreat_amt/100
# 
# #select out only sites that manip both temp and precip
# expclim3<-expclim2a[expclim2a$site=="exp01"|expclim2a$site=="exp05"|expclim2a$site=="exp09"|expclim2a$site=="exp12",]
# #expclim3$target_cent<-scale(expclim3$target, center = TRUE, scale = TRUE)
# #expclim3$preciptreat_amt_cent<-scale(expclim3$preciptreat_amt, center = TRUE, scale = TRUE)
# expclim3<- expclim3 [apply(expclim3 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
# 
# #fit model in lmer
# ###Fit lmer model for soil moisture~warming*preciptreatment
# sm_mod<-lmer(soilmois1~target*preciptreat_prop + (target*preciptreat_prop|site)+(1|year/doy), REML=FALSE, data=expclim3)
# summary(sm_mod)#doesn't converge for preciptreat_amt or precipttreat_prop with expclim2a but DOES for expclim3
# 
# smtemp_mod<-lmer(soilmois1~target + (target|site)+(1|year/doy), REML=FALSE, data=expclim2a)
# summary(smtemp_mod)
# 
# smprecip_mod<-lmer(soilmois1~preciptreat_prop + (preciptreat_prop|site)+(1|year/doy), REML=FALSE, data=expclim2a)
# summary(smprecip_mod)
# 
# #sm_mod_cent<-lmer(soilmois1~target_cent*preciptreat_amt_cent + 
# #                    (target_cent*preciptreat_amt_cent|site)+(1|year/doy),
# #                 REML=FALSE, data=expclim2a)
# #summary(sm_mod_cent)#convergence warning for both expclim3 and expclim 2a
# 
# #sm_mod_cent.b<-brm(soilmois1~target_cent*preciptreat_amt_cent + 
# #             ((target_cent*preciptreat_amt_cent)|site)+(1|year/doy),
# #            data=expclim3)#
# sm_mod.b<-brm(soilmois1~target*preciptreat_prop + 
#                 (target*preciptreat_prop|site)+(1|year/doy),
#               data=expclim3,
#               chains = 2)#
# 
# smtemp_mod.b<-brm(soilmois1~target + 
#                     (target|site)+(1|year/doy),
#                   data=expclim2a)#
# smprecip_mod.b<-brm(soilmois1~preciptreat_prop + 
#                       (preciptreat_prop|site)+(1|year/doy),
#                     data=expclim2a)#
# 
# #summary(sm_mod_cent.b)
# #temp mod
# temp_mod_cent<-lmer(agtemp_mean ~ target_cent*preciptreat_amt_cent +#fixed effects
#                       (target_cent*preciptreat_amt_cent|site) + (1|year/doy), #random effects
#                     data=expclim2a)# control = list(max_treedepth = 15,adapt_delta = 0.99)
# 
# #fit model with brms
# temp_mod.b<-brm(agtemp_mean ~ target*preciptreat_prop +#fixed effects
#                   (target*preciptreat_prop|site) + (1|year/doy), #random effects
#                 data=expclim3,
#                 chains = 2)# control = list(max_treedepth = 15,adapt_delta = 0.99)
# 
# summary(sm_mod_cent.b)
# #save coefficients from models to make a table
# #table<-cbind(fixef(temp_mod_cent.b),fixef(sm_mod_cent.b))
# table<-cbind(summary(temp_mod_cent)$coefficients[,1:2],summary(sm_mod_cent)$coefficients[,1:2])
# table<-round(table, digits=3)
# #sm_tempmod_cent<-lmer(soilmois1~target_cent + (target_cent|site)+(1|year/doy), REML=FALSE, data=expclim2a)
# #summary(sm_tempmod_cent)#model does not fit for noncentered data
# #AIC(sm_tempmod_cent,sm_mod_cent)
# colnames(table)<-c("Tmod.coef","Tmod.se","SMmod.coef","SMmod.se")
# rownames(table)<-c("int","temp.treat","precip.treat","temp.treat*precip.treat")
# write.csv(table,"Analyses/soilmoisture/tempmoislmer.csv", row.names = TRUE)
# 
# 
# #Figure with lines for each site showing different effects
# quartz(height=7, width=7)
# par(mfrow=c(2,2))
# #first air temp
# plot(expclim2a$target_cent[1],expclim2a$agtemp_mean[1],type="p", pch=21,col="white", ylab="Above-ground temperature", xlab="Target warming (centered)", bty="l", xlim=c(-1.5,2.5),ylim=c(0,30))
# #site effects
# cols <- brewer.pal(12,"Set3")
# 
# for(i in 1:dim(coef(temp_mod_cent)$site)[1]){
#   abline(a=coef(temp_mod_cent)$site[i,1],b=coef(temp_mod_cent)$site[i,2], lwd=1, col= cols[i])
# }
# #main effects
# abline(a=fixef(temp_mod_cent)[1],b=fixef(temp_mod_cent)[2], lwd=2)
# 
# plot(expclim2a$preciptreat_amt_cent[1],expclim2a$agtemp_mean[1],type="p", pch=21,col="white", ylab="Above-ground temperature", xlab="Precipitation treatment (centered)", bty="l", xlim=c(-2.5,5),ylim=c(0,30))
# #site effects
# for(i in 1:dim(coef(temp_mod_cent)$site)[1]){
#   abline(a=coef(temp_mod_cent)$site[i,1],b=coef(temp_mod_cent)$site[i,3], lwd=1, col= cols[i])
# }
# abline(a=fixef(temp_mod_cent)[1],b=fixef(temp_mod_cent)[3], lwd=2)
# 
# #soil moisture
# plot(expclim2a$target_cent[1],expclim2$soilmois1[1],type="p", pch=21,col="white", ylab="Soil moisture", xlab="Target warming (centered)", bty="l", ylim=c(0,0.4),xlim=c(-1.5,2.5))
# 
# for(i in 1:dim(coef(sm_mod_cent)$site)[1]){
#   abline(a=coef(sm_mod_cent)$site[i,1],b=coef(sm_mod_cent)$site[i,2], lwd=1, col= cols[i])
# }
# #main effects
# abline(a=fixef(sm_mod_cent)[1],b=fixef(sm_mod_cent)[2], lwd=2)
# 
# plot(expclim2a$preciptreat_amt_cent[1],expclim2a$soilmois1[1],type="p", pch=21,col="white", ylab="Soil moisture", xlab="Precipitation treatment (centered)", bty="l", xlim=c(-2.5,5),ylim=c(0,0.4))
# #site effects
# for(i in 1:dim(coef(sm_mod_cent)$site)[1]){
#   abline(a=coef(sm_mod_cent)$site[i,1],b=coef(sm_mod_cent)$site[i,3], lwd=1, col= cols[i])
# }
# #main effects
# abline(a=fixef(sm_mod_cent)[1],b=fixef(sm_mod_cent)[3], lwd=2)
# #Need to add interaction somehow..
# sites<-unique(expclim2a$site)
# op<-par(cex=.4)
# legend("bottomright",legend=paste(sites),lwd=1,col=cols[1:length(sites)], bty="n")
# 
# expclim2$preciptreat_prop<-expclim2$preciptreat_amt/100
# #now fit a model of
# #target warming:
# sm_mod_targ<-lmer(soilmois1~target*preciptreat_prop + (target*preciptreat_prop|site)+(1|year/doy), REML=FALSE, data=expclim2)
# summary(sm_mod_targ)
# 
# #measured warming:
# sm_mod_meas<-lmer(soilmois1~agtemp_mean*preciptreat_prop + (agtemp_mean*preciptreat_prop|site)+(1|year/doy), REML=FALSE, data=expclim2)
# summary(sm_mod_meas)
# ###########################
# ############################
# #Fit models of soilmois~measured warming with predictor of treatment type as well 
# #use measured warming and predictor for treatment (control, warmed only, warmedwith precip)
# ###########################
# ###########################
# #create a new varibale for treatment type (ttype)
# expclim2$ttype<-"cont"
# expclim2$ttype[as.numeric(expclim2$temptreat)>0]<-"warmprecip"
# expclim2$ttype[expclim2$preciptreat==-1 ]<-"warmprecip"
# expclim2$ttype[expclim2$preciptreat==1]<-"warmprecip"
# expclim2$ttype2<-"cont"
# expclim2$ttype2[as.numeric(expclim2$temptreat)>0]<-"warm"
# expclim2$ttype2[expclim2$preciptreat==-1 ]<-"precip"
# expclim2$ttype2[expclim2$preciptreat==1]<-"precip"
# 
# #do we want to separate out precip only treatments?
# expclim2$ttype<-as.factor(expclim2$ttype)
# expclim2$ttype2<-as.factor(expclim2$ttype2)
# 
# expclim2$agtemp_mean<-as.numeric(expclim2$agtemp_mean)
# expclim2$soilmois1<-as.numeric(expclim2$soilmois1)
# #divide into 2 different datasets
# expclim_cont<-expclim2[expclim2$ttype=="cont",]
# expclim_treat<-expclim2[expclim2$ttype=="warmprecip",]
# #remove exp02, which doesn't measure airtemp
# expclim_cont<-expclim_cont[!expclim_cont$site=="exp02",]
# expclim_treat<-expclim_treat[!expclim_treat$site=="exp02",]
# 
# #expclim_precip<-expclim2[expclim2$ttype=="precip",]
# 
# sm_mod_temptreat<-lmer(soilmois1~agtemp_mean*ttype + (agtemp_mean*ttype|site)+(1|year/doy), REML=FALSE, data=expclim2)
# summary(sm_mod_temptreat)#no convergence
# #Fit separate model for each treatment type for now?
# 
# sm_mod_temptreat_cont<-lmer(soilmois1~agtemp_mean+ (agtemp_mean|site)+(1|year/doy), REML=FALSE, data=expclim_cont)
# summary(sm_mod_temptreat_cont)
# 
# sm_mod_temptreat_treat<-lmer(soilmois1~agtemp_mean + (agtemp_mean|site)+(1|year/doy), REML=FALSE, data=expclim_treat)
# summary(sm_mod_temptreat_treat)
# 
# #sm_mod_temp.brms<-brm(soilmois1~agtemp_mean 
# #                      + (agtemp_mean|site)+(1|year/doy), 
# #                     data=expclim2)
# #summary(sm_mod_temp.brms)
# 
# #Make a plot of soilmois~agtemp, with lines the length of measured climate
# #different lines for each site
# sites_cont<-rownames(ranef(sm_mod_temptreat_cont)$site)
# sites_treat<-rownames(ranef(sm_mod_temptreat_treat)$site)
# 
# #the sites are the same
# #different sites get different colors
# cols <- brewer.pal(8,"Set3")
# 
# quartz(height=7, width=7)
# plot(expclim_cont$target_cent[1],expclim_cont$agtemp_mean[1],type="p", pch=21,col="white", ylab="Soil moisture", xlab="Above-ground temperature", bty="l", xlim=c(min(expclim2$agtemp_mean, na.rm=TRUE)-.5,max(expclim2$agtemp_mean, na.rm=TRUE)),ylim=c(-0,0.5))
# 
# #add lines for models fit to control plots
# for(i in 1:length(sites_cont)){
#   site.mod<-coef(sm_mod_temptreat_cont)$site[i,]
#   xsitemax<-max(expclim_cont$agtemp_mean[expclim_cont==sites_cont[i]], na.rm=TRUE) 
#   xsitemin<-min(expclim_cont$agtemp_mean[expclim_cont==sites_cont[i]], na.rm=TRUE) 
#   ysitemax<-max(expclim_cont$soilmois1[expclim_cont==sites_cont[i]], na.rm=TRUE) 
#   ysitemin<-min(expclim_cont$soilmois1[expclim_cont==sites_cont[i]], na.rm=TRUE) 
#   ablineclip(a=as.numeric(site.mod[1]),b=as.numeric(site.mod[2]),x1=xsitemin,x2=xsitemax,y1=-0.5,y2=1.0,col=cols[i])
#   print(site.mod);
#   print(xsitemax); print(xsitemin);
#   
#   #xvals<-as.vector(seq(xsitemin,xsitemax))
#   #yvals<-as.numeric(site.mod[2])*(xvals)+as.numeric(site.mod[1])
#   #lines(xvals,yvals,col=cols[i], lty=3)
# }
# #main line for control plots
# xmax<-max(expclim_cont$agtemp_mean, na.rm=TRUE) 
# xmin<-min(expclim_cont$agtemp_mean, na.rm=TRUE) 
# 
# ablineclip(fixef(sm_mod_temptreat_cont),lwd=2,col="black", x1=xmin, x2=xmax)
# #now add treatment plots
# for(i in 1:length(sites_treat)){
#   site.mod<-coef(sm_mod_temptreat_treat)$site[i,]
#   xsitemax<-max(expclim_treat$agtemp_mean[expclim_treat==sites_treat[i]], na.rm=TRUE) 
#   xsitemin<-min(expclim_treat$agtemp_mean[expclim_treat==sites_treat[i]], na.rm=TRUE) 
#   ysitemax<-max(expclim_treat$soilmois1[expclim_treat==sites_treat[i]], na.rm=TRUE) 
#   ysitemin<-min(expclim_treat$soilmois1[expclim_treat==sites_treat[i]], na.rm=TRUE) 
#   ablineclip(a=as.numeric(site.mod[1]),b=as.numeric(site.mod[2]),x1=xsitemin,x2=xsitemax,y1=-0.5,y2=1.0,col=cols[i], lty=2)
#   print(site.mod);
#   print(xsitemax); print(xsitemin);
# }
# xmax2<-max(expclim_treat$agtemp_mean, na.rm=TRUE) 
# xmin2<-min(expclim_treat$agtemp_mean, na.rm=TRUE) 
# 
# ablineclip(fixef(sm_mod_temptreat_treat),lwd=2,col="black", lty=2, x1=xmin2, x2=xmax2)
# ###############
# #Try divide into 3 different datasets
# expclim_cont2<-expclim2[expclim2$ttype2=="cont",]
# expclim_warm<-expclim2[expclim2$ttype2=="warm",]
# 
# #remove exp02, which doesn't measure airtemp
# expclim_cont2<-expclim_cont2[!expclim_cont2$site=="exp02",]
# expclim_warm<-expclim_warm[!expclim_warm$site=="exp02",]
# 
# 
# 
# 
# 
# #########Exploring#########
# 
# sm_mod_temp_cont<-lmer(soilmois1~agtemp_mean + (agtemp_mean|site)+(1|year/doy), REML=FALSE, data=expclim_cont)
# summary(sm_mod_temp_cont)
# 
# modcomp<-rbind(fixef(sm_mod_targ)[,1:2],fixef(sm_mod_meas)[,1:2],c(fixef(sm_mod_temp)[,1],NA,NA),c(fixef(sm_mod_temp)[,2],NA,NA),c(fixef(sm_mod_temp_cont)[,1],NA,NA),c(fixef(sm_mod_temp_cont)[,2],NA,NA))
# 
# colnames(modcomp)<-c("eff","se")
# 
#Checking inclusion criteria- species had to be present at more than one site
#sen- only 2 sites (5,6), 4/93 species found at both sites
sentab<-table(datalist.sen.cent$site,datalist.sen.cent$sp)
sen<-apply(sentab,2,function(x) sum(x > 0))
length(sen[which(sen>1)])
length(sen)
#fr- 3 sites (2,6,8), 0/77 species found at >1 site
frdtab<-table(datalist.ffrd.cent$site,datalist.ffrd.cent$sp)
fr<-apply(frdtab,2,function(x) sum(x > 0))
length(fr[which(fr>1)])
length(fr)
#fl-5 sites (1,2,6,7,8), 10/125 species found at >1 site
fldtab<-table(datalist.ffd.cent$site,datalist.ffd.cent$sp)
fl<-apply(fldtab,2,function(x) sum(x > 0))
length(fl[which(fl>1)])
length(fl)
# dim(table(datalist.lod.cent$site,datalist.lod.cent$sp))
lodtab<-table(datalist.lod.cent$site,datalist.lod.cent$sp)
lo<-apply(lodtab,2,function(x) sum(x > 0))
length(lo[which(lo>1)])
length(lo)
#lo-5 sites (1,3,4,5,6), 23/138 species found at >1 site
bbdtab<-table(datalist.bbd.cent$site,datalist.bbd.cent$sp)
bb<-apply(bbdtab,2,function(x) sum(x > 0))
length(bb[which(bb>1)])
length(bb)
#bb-5 sites (1,3,4,5,7), 19/41 species found at >1 site

          