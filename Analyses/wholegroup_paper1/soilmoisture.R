#Soil moisture analyses across treatments
#Started 17 January 2017
#Alternative version of 
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(lme4)
setwd("~/git/radcliffe/Analyses")
#expclim<-read.csv("gddchill/expclim.wchillgdd.csv", header=T)
expclim<-read.csv("expclim.csv", header=T)
treats<-read.csv("treats_detail.csv", header=T)
treats$preciptreat<-as.character(treats$preciptreat)
effwarm.plot$preciptreat<-as.character(effwarm.plot$preciptreat)
treats$temptreat<-as.character(treats$temptreat)
effwarm.plot$temptreat<-as.character(effwarm.plot$temptreat)
treats$site<-as.character(treats$site)
effwarm.plot$site<-as.character(effwarm.plot$site)
treats$plot<-as.character(treats$plot)
effwarm.plot$plot<-as.character(effwarm.plot$plot)

effwarm.plot <- read.csv("EffectiveWarming_Plot.csv", header=TRUE)#i think the treats file and effective warming should have the same number of rows- one per site-plot, right? why aren't they matching up?
effwarm.plot<-effwarm.plot[,-which(colnames(effwarm.plot)=="block")]
exp.tarrep <- left_join(treats,effwarm.plot, by=c("site", "plot","temptreat","preciptreat"))
exp.tarrep[which(is.na(exp.tarrep$AG.type)),]

#  monthdat$temptreat <- relevel(as.factor( monthdat$temptreat), ref = "ambient")
moismod<-lmer(soilmois1~temptreat + (temptreat|site/year), data=expclim_cont2, REML=FALSE)
