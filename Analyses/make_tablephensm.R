#Make table of sites, phenophases, airtemp, soiltemp etc
library(dplyr)
if(length(grep("ailene", getwd()))>0) {setwd("~GitHub/radcliffe")}
setwd("C:/Users/ailene.ettinger.TNC/OneDrive - The Nature Conservancy/Documents/GitHub/radcliffe")

#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)

remove.conifers=TRUE
use.airtemp=TRUE
use.centmod=TRUE
#data of interest for plotting (phenophase, site, sp, if applicable)
#standard data wrangling to get expclim2 for climate analyses and expgdd
#for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")

#summarize climate data by plot (annual and seasonal temp, soil mois), 
#merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
source("Analyses/source/climsum_byplot.R")

#Prepare data for phenology models in stan
source("Analyses/source/stanprep_phenmods.R")


#load budburst model

if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==TRUE){
  load("Analyses/output/brms/testm5cent.brms.bb.Rda")
  mod<-testm5cent.brms}

summary(mod)
names(mod)
bbtab<-round(fixef(mod,probs = c(0.25,0.75,0.025, 0.975)), digits = 2)

row.names(bbtab)<-c("$\\mu_{\\alpha}$","$\\mu_{temp}$","$\\mu_{mois}$",   
                    "$\\mu_{temp:mois}$")
colnames(bbtab)<- c("mean",'error',"25%", "75%","2.5%","97.5%")
#groups

sigmas<-round(rbind(summary(mod)$random$site[1:2],summary(mod)$random$`site:year`[1:2],summary(mod)$random$sp[1:4,1:2]), digits=2)
ns<-rbind(summary(mod)$ngrps$site,summary(mod)$ngrps$`site:year`,summary(mod)$ngrps$sp,"","","")
bbtab_ran<-cbind(c("site","site:year","species","","",""),
        c("$\\sigma_{Isite}$","$\\sigma_{siteyr}$", "$\\sigma_{sp}$", "$\\sigma_{temp_{sp}}$", "$\\sigma_{mois_{sp}}$","$\\sigma_{temp*mois_{sp}}$"),
                    sigmas,ns)
rownames(bbtab_ran)<-NULL

ysigma<-c("$\\sigma_{y}$",summary(mod)$spec_pars[1:2],summary(mod)$nobs)


#add leafout
if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==TRUE){
  load("Analyses/output/brms/testm5cent.brms.lo.Rda")
  mod<-testm5cent.lod.brms
  summary(mod)
  mod$fit
  lotab<-round(fixef(mod,probs = c(0.25,0.75,0.025, 0.975)), digits = 2)
  rownames(nonztab)[10]<-"n_sp"
  row.names(lotab)<-c("$\\mu_{\\alpha}$","$\\mu_{temp}$","$\\mu_{mois}$",   
                      "$\\mu_{temp:mois}$","$\\sigma_{\\alpha}$", "$\\sigma_{forcing}$"
                      , "$\\sigma_{photoperiod}$","$\\sigma_{chilling}$","$\\sigma_{y}$","$N_{sp}$")
  colnames(lotab)<- c("mean","25%", "75%","2.5%","97.5%")
}




if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==TRUE){
  load("Analyses/output/brms/gddmbb.Rda")
  gddmod<-gddmbb.brms}


#add leafout, flowering to same table
#alltab<-rcbind(bbtab, lotab,fltab)

