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

#try the model with brms
testm5.brms <- brm(y ~ temp * mois +#fixed effects
                     (temp * mois|sp) + (1|site/year), #random effects
                   data=datalist.bbd,iter= 3000,
                   chains = 2, control = list(max_treedepth = 15,adapt_delta = 0.99))

# Code if you want to save your models (do NOT push output to git)
save(testm5.brms, file="Analyses/output/brms/testm5.brms.bb.Rda")

# #Fit model with control data only
# testm5cont.brms <- brm(y ~ temp * mois +#fixed effects
#                          (temp * mois|sp) + (1|site/year), #random effects
#                        data=datalist.bbdcont,
#                        chains = 2)# control = list(max_treedepth = 15,adapt_delta = 0.99)
# 
# 
# 
testm5cent.brms <- brm(y ~ temp * mois +#fixed effects
                          (temp * mois|sp) + (1|site/year), #random effects
                        data=datalist.bbd.cent,
                        chains = 2)#control = list(max_treedepth = 15,adapt_delta = 0.99)
save(testm5cent.brms, file="Analyses/output/brms/testm5cent.brms.bb.Rda")


testm5.lod.brms <- brm(y ~ temp * mois +#fixed effects
                         (temp * mois|sp) + (1|site/year), #random effects
                       data=datalist.lod,iter = 3000,
                       chains = 2,control = list(max_treedepth = 15,adapt_delta = 0.99))
save(testm5.lod.brms, file="Analyses/output/brms/testm5.brms.lo.Rda")

testm5cent.lod.brms <- brm(y ~ temp * mois +#fixed effects
                             (temp * mois|sp) + (1|site/year), #random effects
                           data=datalist.lod.cent,
                           chains = 2,control = list(max_treedepth = 15,adapt_delta = 0.99))

save(testm5cent.lod.brms, file="Analyses/output/brms/testm5cent.brms.lo.Rda")

testm5cent.ffd.brms <- brm(y ~ temp * mois +#fixed effects
                             (temp * mois|sp) + (1|site/year), #random effects
                           data=datalist.ffd.cent,
                           chains = 2,control = list(max_treedepth = 15,adapt_delta = .999))
save(testm5cent.ffd.brms, file="Analyses/output/brms/testm5cent.brms.ff.Rda")

testm5cent.ffrd.brms <- brm(y ~ temp * mois +#fixed effects
                              (temp * mois|sp) + (1|site/year), #random effects
                            data=datalist.ffrd.cent,
                            chains = 2,control = list(max_treedepth = 15,adapt_delta = .999))


save(testm5cent.ffr.brms, file="Analyses/output/brms/testm5cent.brms.fr.Rda")


testm5cent.sen.brms <- brm(y ~ temp * mois +#fixed effects
                             (temp * mois|sp) + (1|site/year), #random effects
                           data=datalist.sen.cent,
                           chains = 2,control = list(max_treedepth = 15,adapt_delta = .999))
save(testm5cent.sen.brms, file="Analyses/output/brms/testm5cent.brms.se.Rda")

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