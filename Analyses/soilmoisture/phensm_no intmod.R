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
# Fit no into mod to data #
##################
testmcent.brms <- brm(y ~ temp + mois +#fixed effects
                        (temp + mois|sp) + (1|site/year), #random effects
                      data=datalist.bbd.cent,  chains = 4,iter = 4000)
save(testmcent.brms, file="Analyses/output/brms/testmcent.noint.brms.bb.Rda")
round(fixef(testmcent.brms, probs=c(.90,0.10)), digits=2)
#`107 divergent transitions!`