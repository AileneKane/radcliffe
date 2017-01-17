#Soil moisture analyses across treatments
#Started 17 January 2017
#Alternative version of 
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(lme4)
setwd("~/git/radcliffe/Analyses")
#expclim<-read.csv("gddchill/expclim.wchillgdd.csv", header=T)
expclim<-read.csv("expclim.csv", header=T)


#  monthdat$temptreat <- relevel(as.factor( monthdat$temptreat), ref = "ambient")
moismod<-lmer(soilmois1~temptreat + (temptreat|site/year), data=expclim_cont2, REML=FALSE)
