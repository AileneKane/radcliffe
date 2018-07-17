#Started Sept 2017
#By Ailene
#Use brms/Stan to fit soil moisture- phenology model to radcliffe data 
#This code focuses on the following question
#3) Does warming affect soil moisture (and phenology) similarly in experimental and non-experimental data?
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

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#options(mc.cores = parallel::detectCores()) # added by Andrew
#update.packages()

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/radcliffe")}
#Harvard forest (HF) soil data (1995-2010): collected every 1-2 weeks.
hfsoil<-read.csv("Data/Other/HFObsClimData/hf006-01-soil-respiration.csv", header=TRUE)
#HF air temp data (1991-2014)
hfair<-read.csv("Data/Other/HFObsClimData/hf004-02-filled.csv", header=TRUE)
#HF precip data (also includes air temp- from the climatological station, 2001-2014)
hfclim<-read.csv("Data/Other/HFObsClimData/hf001-04-monthly-m.csv", header=TRUE)
#how to compare?


#DF soil moisture data (2000-2018): collected every 2-4 weeks, from 4 sites.
#OR
#DF Wisard data (2006-2012)- matches the air temp data 
soilfiles<-
dfsoil1<-read.csv("Data/Other/DukeObsClimData/HW_wisard_TDR.csv", header=TRUE)
#DF air temp data (2006-2018).

#start by summarizing air temp across years, to compare affect on soil moisture

#This is the model I want to compare it with, for experimetnal data:
#summary(sm_mod)
#sm_mod_cent<-lmer(soilmois1~target_cent*preciptreat_amt_cent + (target_cent*preciptreat_amt_cent|site)+(1|year/doy), REML=FALSE, data=expclim2a)
#summary(sm_mod_cent)
#should probably compare it to actual air temp not target?