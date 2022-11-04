#Make table of sites, phenophases, airtemp, soiltemp etc
library(dplyr)
library(brms)
#if(length(grep("ailene", getwd()))>0) {setwd("~GitHub/radcliffe")}
# #setwd("C:/Users/ailene.ettinger.TNC/OneDrive - The Nature Conservancy/Documents/GitHub/radcliffe")
# #setwd("~/Documents/GitHub/radcliffe")
# #Read in experimental climate and phenology data
# expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
# exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
# treats<-read.csv("Analyses/treats_detail.csv", header=T)
# 
# remove.conifers=TRUE
# use.airtemp=TRUE
# use.centmod=TRUE
# #data of interest for plotting (phenophase, site, sp, if applicable)
# #standard data wrangling to get expclim2 for climate analyses and expgdd
# #for phenology analyses (with gddcrit)
# source("Analyses/source/standard_mergesandwrangling.R")
# 
# #summarize climate data by plot (annual and seasonal temp, soil mois), 
# #merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
# source("Analyses/source/climsum_byplot.R")
# 
# #Prepare data for phenology models in stan
# source("Analyses/source/stanprep_phenmods.R")
# 

#load budburst model

#if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==TRUE){
  load("../../Analyses/output/brms/testm5.brms.bb.Rda")
  mod<-testm5.brms
#  }

summary(mod)
names(mod)
bbtab<-cbind(round(fixef(mod,probs = c(0.25,0.75,0.05, 0.95)), digits = 1),#fixed effects
             cbind(round(summary(mod)$random$sp[1:4,1:2], digits=1),c(summary(mod)$ngrps$sp,"","","")),#random effects of species
             rbind(c(round(summary(mod)$random$site[1:2], digits=1),summary(mod)$ngrps$site),c("","",""),c("","",""),c("","","")),#ran effects of site
             rbind(c(round(summary(mod)$random$`site:year`[1:2],digits=1),summary(mod)$ngrps$`site:year`),c("","",""),c("","",""),c("","",""))#random effects of site:year
             
)
row.names(bbtab)<-c("BB$\\mu_{\\alpha}$","BB$\\mu_{temp}$","BB$\\mu_{mois}$",   
                    "BB$\\mu_{temp:mois}$")
colnames(bbtab)<- c("mean",'error',"25%", "75%","5%","95%","mean",'error',"Ngrp","mean",'error',"Ngrp","mean",'error',"Ngrp")

#bbtab_ran<-cbind(c("site","site:year","species","","",""),
       # c("$\\sigma_{Isite}$","$\\sigma_{siteyr}$", "$\\sigma_{sp}$", "$\\sigma_{temp_{sp}}$", "$\\sigma_{mois_{sp}}$","$\\sigma_{temp*mois_{sp}}$"),
#bbysigma<-c("$\\sigma_{y}$",summary(mod)$spec_pars[1:2],summary(mod)$nobs)

#add leafout
#if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==TRUE){
  load("../../Analyses/output/brms/testm5.brms.lo.Rda")
  mod<-testm5.lod.brms
  
 lotab<-cbind(round(fixef(mod,probs = c(0.25,0.75,0.05, 0.95)), digits = 1),#fixed effects
              cbind(round(summary(mod)$random$sp[1:4,1:2], digits=1),c(summary(mod)$ngrps$sp,"","","")),#random effects of species
               rbind(c(round(summary(mod)$random$site[1:2], digits=1),summary(mod)$ngrps$site),c("","",""),c("","",""),c("","","")),#ran effects of site
               rbind(c(round(summary(mod)$random$`site:year`[1:2],digits=1),summary(mod)$ngrps$`site:year`),c("","",""),c("","",""),c("","",""))#radnom effects of site:year
               
  )
  row.names(lotab)<-c("LO$\\mu_{\\alpha}$","LO$\\mu_{temp}$","LO$\\mu_{mois}$",   
                      "LO$\\mu_{temp:mois}$")
  colnames(lotab)<- c("mean",'error',"25%", "75%","5%","95%","mean",'error',"Ngrp","mean",'error',"Ngrp","mean",'error',"Ngrp")
  

#add flowerin
#if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==TRUE){
load("../../Analyses/output/brms/testm5.brms.ff.Rda")
mod<-testm5.ffd.brms
#summary(mod)
#mod$fit
fltab<-cbind(round(fixef(mod,probs = c(0.25,0.75,0.05, 0.95)), digits = 1),#fixed effects
             cbind(round(summary(mod)$random$sp[1:4,1:2], digits=1),c(summary(mod)$ngrps$sp,"","","")),#random effects of species
             rbind(c(round(summary(mod)$random$site[1:2], digits=1),summary(mod)$ngrps$site),c("","",""),c("","",""),c("","","")),#ran effects of site
             rbind(c(round(summary(mod)$random$`site:year`[1:2],digits=1),summary(mod)$ngrps$`site:year`),c("","",""),c("","",""),c("","",""))#random effects of site:year
             
)
row.names(fltab)<-c("FL$\\mu_{\\alpha}$","FL$\\mu_{temp}$","FL$\\mu_{mois}$",   
                    "FL$\\mu_{temp:mois}$")
colnames(fltab)<- c("mean",'error',"25%", "75%","5%","95%","mean",'error',"Ngrp","mean",'error',"Ngrp","mean",'error',"Ngrp")
#flysigma<-c("$\\sigma_{y}$",summary(mod)$spec_pars[1:2],summary(mod)$nobs)

#add leafout, flowering to same table

alltab<-rbind(bbtab,lotab,fltab)
