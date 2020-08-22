#Make table of sites, phenophases, airtemp, soiltemp etc
library(dplyr)
if(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/radcliffe")}

#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)

remove.conifers=TRUE
use.airtemp=TRUE
use.centmod=TRUE
#data of interest for plotting (phenophase, site, sp, if applicable)
phen="BB"#options that work are "BB" or "LO"
#standard data wrangling to get expclim2 for climate analyses and expgdd
#for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")

#summarize climate data by plot (annual and seasonal temp, soil mois), 
#merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
source("Analyses/source/climsum_byplot.R")

#Prepare data for phenology models in stan
source("Analyses/source/stanprep_phenmods.R")


#load budburst model
if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==FALSE & phen=="BB"){
  load("Analyses/output/brms/testm5.rstanarm.bb.Rda")
  mod<-testm5.rstan
  }
if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==TRUE & phen=="BB"){
  load("Analyses/output/brms/testm5cent.brms.bb.Rda")
  mod<-testm5cent.brms}

summary(mod)
mod$fit
bbtab<-round(fixef(mod,probs = c(0.25,0.75,0.025, 0.975)), digits = 2)
rownames(nonztab)[10]<-"n_sp"
row.names(bbtab)<-c("$\\mu_{\\alpha}$","$\\mu_{temp}$","$\\mu_{mois}$",   
                    "$\\mu_{temp:mois}$","$\\sigma_{\\alpha}$", "$\\sigma_{forcing}$"
                    , "$\\sigma_{photoperiod}$","$\\sigma_{chilling}$","$\\sigma_{y}$","$N_{sp}$")
colnames(bbtab)<- c("mean","25%", "75%","2.5%","97.5%")
#add leafout
if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==TRUE & phen=="BB"){
  load("Analyses/output/brms/testm5cent.brms.lo.Rda")
  mod<-testm5cent.lod.brms
  summary(mod)
  mod$fit