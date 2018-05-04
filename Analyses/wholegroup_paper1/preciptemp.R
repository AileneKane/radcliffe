#Additional analyses for Experimental Climate Paper
#Analysis of precipitation treatment effects on temperature
#By Ailene Ettinger
#Started 7 november 2017
library(xtable)
library(lme4)
library(car)
require(dplyr)
#include only studies that modify precip
expclim<-read.csv("../../Analyses/expclim.csv", header=T)
treats<-read.csv("../../Analyses/treats_detail.csv", header=T)
treats[which(is.na(treats$target)),]$target<-0
treats[which(is.na(treats$preciptreat_amt)),]$preciptreat_amt<-100
expclim2<-left_join(expclim,treats, by=c("site", "block", "plot","temptreat","preciptreat"), match="all", copy=TRUE)
#which studies manupulate precip
expclimp<-expclim2[which(expclim2$site=="exp01"|expclim2$site=="exp02"|expclim2$site=="exp05"|expclim2$site=="exp09"|expclim2$site=="exp12"),]#studies that manipulated precip

#get one column for above-ground temperature
expclimp$agtemp_min<-expclimp$airtemp_min
expclimp[which(is.na(expclimp$agtemp_min) & !is.na(expclimp$cantemp_min)),]$agtemp_min <-expclimp[which(is.na(expclimp$airtemp_min) & !is.na(expclimp$cantemp_min)),]$cantemp_min

expclimp[which(is.na(expclimp$agtemp_min) & !is.na(expclimp$surftemp_min)),]$agtemp_min<-expclimp[which(is.na(expclimp$agtemp_min) & !is.na(expclimp$surftemp_min)),]$surftemp_min

expclimp$agtemp_max<-expclimp$airtemp_max

expclimp[which(is.na(expclimp$agtemp_max) & !is.na(expclimp$cantemp_max)),]$agtemp_max<-expclimp[which(is.na(expclimp$airtemp_max) & !is.na(expclimp$cantemp_max)),]$cantemp_max

expclimp[which(is.na(expclimp$agtemp_max) & !is.na(expclimp$surftemp_max)),]$agtemp_max<-expclimp[which(is.na(expclimp$agtemp_max) & !is.na(expclimp$surftemp_max)),]$surftemp_max
#change precip to proportion
#expclimp$preciptreat_amt<-as.numeric(expclimp$preciptreat_amt)/100
expclimp<-subset(expclimp,select=c(site,year,doy,preciptreat_amt,target,agtemp_min,agtemp_max,soiltemp1_min,soiltemp1_max))
expclimp<- expclimp [apply(expclimp , 1, function(x) all(!is.na(x))),]
tempmod4<-lmer(agtemp_max~preciptreat_amt * target+ (1|site/year/doy), data=expclimp, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
tempmod4_min<-lmer(agtemp_min~preciptreat_amt * target+ (1|site/year/doy), data=expclimp, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
mintemptable<-cbind(round(summary(tempmod4_min)$coeff[,1:2],digits=3), Anova(tempmod4_min, type="III"))
temptable<-cbind(round(summary(tempmod4)$coeff[,1:2],digits=3), Anova(tempmod4, type="III"))
stempmod4<-lmer(soiltemp1_min~preciptreat_amt * target+ (1|site/year/doy), data=expclimp, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
stempmod4_min<-lmer(soiltemp1_max~preciptreat_amt * target+ (1|site/year/doy), data=expclimp, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
smintemptable<-cbind(round(summary(stempmod4_min)$coeff[,1:2],digits=3), Anova(stempmod4_min, type="III"))
stemptable<-cbind(round(summary(stempmod4)$coeff[,1:2],digits=3), Anova(stempmod4, type="III"))
#which studies?
#unique(expclimp$site)
alltemptable<-as.data.frame(rbind(mintemptable,temptable,smintemptable,stemptable))
mods<-c("min above-ground temp.","","","","max above-ground temp.","","","","min soil temp.","","","","max soil temp.","","","")
preds<-c("intercept","preciptreat","warmtreat","precip*warm","intercept","preciptreat","warmtreat","precip*warm","intercept","preciptreat","warmtreat","precip*warm","intercept","preciptreat","warmtreat","precip*warm")
alltemptable<-cbind(mods,preds,alltemptable)
rownames(alltemptable)<-NULL
colnames(alltemptable)<-c("response","predictors","est.","se","$\\chi^{2}$","df","p")
alltemptable$p[as.numeric(alltemptable$p)<0.001]<-"<0.001"
alltemptable$df<-as.character("1")
