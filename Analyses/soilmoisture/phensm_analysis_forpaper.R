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
rstan_options(disable_march_warning = TRUE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) {setwd("~/GitHub/radcliffe")}

#setwd("~/Documents/GitHub/radcliffe")

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

mean(datalist.bbd$mois)
#try the model with no interactgions
testmcent.brms <- brm(y ~ temp + mois +#fixed effects
                         (temp + mois|sp) + (1|site/year), #random effects
                       data=datalist.bbd.cent,
                       chains = 2,iter = 2000,
                       control = list(max_treedepth = 15,adapt_delta = 0.999))
save(testmcent.brms, file="Analyses/output/brms/testmcentnoint.brms.bb.Rda")
round(fixef(testmcent.brms, probs=c(.90,0.10)), digits=2)
#needs more iteratuons...but from estinates temp effect is sinilar, mois effect is a little weaker but similar

#try the model with brms
testm5cent.brms <- brm(y ~ temp * mois +#fixed effects
                     (temp * mois|sp) + (1|site/year), #random effects
                   data=datalist.bbd.cent,
                   chains = 4,iter = 4000,
                   control = list(max_treedepth = 15,adapt_delta = 0.999), paralell=TRUE)

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
                       chains = 4,iter = 4000,
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
                       chains = 4,iter = 4000,
                       control = list(max_treedepth = 15,adapt_delta = 0.99))
save(testm5cent.lod.brms, file="Analyses/output/brms/testm5cent.brms.lo.Rda")
round(fixef(testm5cent.lod.brms, probs=c(.90,0.10)), digits=2)

#now fit with data containing same species as lo
#try the model with brms
testm5cent.lodbb.brms <- brm(y ~ temp * mois +#fixed effects
                         (temp * mois|sp) + (1|site/year), #random effects
                       data=datalist.lodbb.cent,
                       chains = 4,iter = 4000,
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
                             chains = 4,iter = 4000,
                             control = list(max_treedepth = 15,adapt_delta = 0.9999))
save(testm5cent.lodfl.brms, file="Analyses/output/brms/testm5cent.brms.lofl.Rda")
#1 divergent transition of warmup with 

testm5cent.ffd.brms <- brm(y ~ temp * mois +#fixed effects
                             (temp * mois|sp) + (1|site/year), #random effects
                           data=datalist.ffd.cent,
                           chains = 4,iter = 4000,
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
                           chains = 4,iter = 5000,
                           control = list(max_treedepth = 15,adapt_delta = .9999))
save(testm5cent.ffrd.brms, file="Analyses/output/brms/testm5cent.brms.frd.Rda")
round(fixef(testm5cent.ffrd.brms, probs=c(.90,0.10)), digits=2)


testm5cent.sen.brms <- brm(y ~ temp * mois +#fixed effects
                              (temp * mois|sp) + (1|site/year), #random effects
                            data=datalist.sen.cent,
                            chains = 4,iter = 6000,
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
                       chains = 4,iter = 4000,
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
                   chains = 4,iter = 4000,
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
                   chains = 4,iter = 4000,
                   control = list(max_treedepth = 15,adapt_delta = 0.99))
save(testm5.brms, file="Analyses/output/brms/testm5.brms.lo.Rda")
round(fixef(testm5.brms, probs=c(.90,0.10)), digits=2)

#
save(testm5.brms, file="Analyses/output/brms/testm5.brms.bb.Rda")
round(fixef(testm5.brms, probs=c(.90,0.10)), digits=2)

#############################
####     GDD models     #####
#############################
source("Analyses/source/stanprep_gddmods.R")

#Budburst

gddmbb.brms <- brm(y ~ mois +#fixed effects
                               (mois|sp) + (1|site/year), #random effects
                             data=datalist.gddbb,
                             chains = 2,iter = 3000,control = list(max_treedepth = 15,adapt_delta = 0.99))

save(gddmbb.brms, file="Analyses/output/brms/gddmbb.Rda")
round(fixef(gddmbb.brms, probs=c(.90,0.10)), digits=2)
ranef(gddmbb.brms)
summary(gddmbb.brms)

plot(gddmbb.brms)
conditional_effects(gddmbb.brms)


#Leafout
gddmlo.brms <- brm(y ~ mois +#fixed effects
                     (mois|sp) + (1|site/year), #random effects
                   data=datalist.gddlo,
                   chains = 2,iter = 4000,control = list(max_treedepth = 15,adapt_delta = 0.99))
save(gddmlo.brms, file="Analyses/output/brms/gddmlo_ranslope.Rda")
round(fixef(gddmlo.brms, probs=c(.90,0.10)), digits=2)
summary(gddmlo.brms)

plot(gddmlo.brms)
conditional_effects(gddmlo.brms)

#Flowering
gddmfl.brms <- brm(y ~ mois +#fixed effects
                     (mois|sp) + (1|site/year), #random effects
                   data=datalist.gddfl,
                   chains = 2,iter = 5000,control = list(max_treedepth = 15,adapt_delta = 0.99))
save(gddmfl.brms, file="Analyses/output/brms/gddmfl_ranslope.Rda")
round(fixef(gddmfl.brms, probs=c(.90,0.10)), digits=2)
summary(gddmfl.brms)

plot(gddmfl.brms)
conditional_effects(gddmfl.brms)

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

          