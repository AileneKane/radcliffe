#Summaries of linear mixed-effects models comparing effects of ambient versus structural controls on daily mean, minimum, and maximum soil temperature in climate change experiments across the year
#Summaries of linear mixed-effects models comparing effects of ambient versus structural controls on daily minimum and maximum air temperature in climate change experiments, across the year.
#Summary of a linear mixed-effects model comparing effects of ambient versus structural controls on daily soil moisture (\\% volumetric water content, VWC) in climate change experiments across the year.
options(stringsAsFactors = FALSE)
library(plyr)
library(dplyr)
library(lme4)
library(car)
library(xtable)
setwd("/Users/aileneettinger/git/radcliffe/Analyses/wholegroup_paper1")
expclim<-read.csv("../../Analyses/expclim.csv", header=TRUE)
exppheno<-read.csv("../../Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("../../Analyses/treats_detail.csv", header=T)

#standard data wrangling to get expclim2 for climate analyses and expgdd for phenology analyses
source("../../Analyses/source/standard_mergesandwrangling.R")

expclim_controls<-expclim2[expclim2$temptreat=="0"|expclim$temptreat=="ambient",]
expclim_controls$temptreat<-factor(expclim_controls$temptreat)
expclim_controls$styear<-factor(expclim_controls$styear)

expclim_controls$airtemp_mean<-(expclim_controls$airtemp_min+expclim_controls$airtemp_max)/2
sitesums<-data.frame(tapply(expclim_controls$soiltemp1_mean,list(expclim_controls$site,expclim_controls$temptreat),length))
colnames(sitesums)<-c("sham.control","ambient")
sites_con<-rownames(sitesums)[!is.na(sitesums$sham.control) & !is.na(sitesums$ambient)]
expclim_controls$year<-as.factor(expclim_controls$year)
expclim_cont<-expclim_controls[expclim_controls$site %in% sites_con,]
#first fit annual models
expclim_cont$temptreat <- relevel(as.factor( expclim_cont$temptreat), ref = "ambient")
smod_mn<-lmer(soiltemp1_mean~temptreat + (temptreat|site/styear), data=expclim_cont, REML=FALSE)
smod_max<-lmer(soiltemp1_max~temptreat + (temptreat|site/styear), data=expclim_cont, REML=FALSE)
smod_min<-lmer(soiltemp1_min~temptreat + (temptreat|site/styear), data=expclim_cont, REML=FALSE)
mod_max<-lmer(airtemp_max~temptreat + (temptreat|site/styear), data=expclim_cont, REML=FALSE)
mod_min<-lmer(airtemp_min~temptreat + (temptreat|site/year), data=expclim_cont, REML=FALSE)
mnstable<-cbind(round(summary(smod_mn)$coeff[,1:2],digits=2))
maxstable<-cbind(round(summary(smod_max)$coeff[,1:2],digits=2))
minstable<-cbind(round(summary(smod_min)$coeff[,1:2],digits=2))
stableAll<-as.data.frame(cbind(mnstable,minstable,maxstable))
rownames(stableAll)<-c("intercept","structure effect")
#stableAll<-stableAll[,-which(colnames(stableAll)=="X")]

write.csv(stableAll,"../../Analyses/wholegroup_paper1/output/stableAll.csv", row.names=FALSE)
stableAll<-read.csv("../../Analyses/wholegroup_paper1/output/stableAll.csv",header=TRUE)
rownames(stableAll)<-c("intercept","structure effect")
stableAll<-stableAll[,-which(colnames(stableAll)=="X")]


stmod.table<-xtable(stableAll,caption="\\textbf{Summaries of linear mixed-effects models comparing effects of ambient versus structural controls on daily mean, minimum, and maximum soil temperature in climate change experiments across the year}. Estimates (est.) are the intercept (representing ambient controls) and coefficient (representing structure effects) from the models; se is the standard error for these estimates. For these annual models, differences between control types were significant based on Type II Wald $\\chi^{2}$ tests of fixed effects for mean soil temperature ($\\chi^{2}$=5.53, df=1, p=0.02) and minimum soil temperature ($\\chi^{2}$=3.87, df=1, p=0.05), but not for maximum soil temperature ($\\chi^{2}$=2.08, df=1, p=0.15). For all models, random effects of site (n=5 for mean model, n=4 for min and max models) and year nested within site (n=21 for mean model, n=20 for min and max models) were fit with a random slope and intercept structure; total number of observations= 48,860 for the mean model and 44,530 for the min and max models; units are \\degree C.",label="table:shamamb_soiltemp",align=c(
                      "p{0.15\\textwidth}",
                      "|p{0.05\\textwidth}",
                      "p{0.05\\textwidth}",
                      "|p{0.05\\textwidth}",
                      "p{0.05\\textwidth}|",
                      "p{0.05\\textwidth}",
                      "p{0.05\\textwidth}|"))

addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("&\\multicolumn{2}{c |}{mean soil temp.} &\\multicolumn{2}{c |}{min soil temp.} &\\multicolumn{2}{c |}{max soil temp.}\\\\\n \\hline","predictor & est. & se & est. & se & est. & se\\\\\n") 
print(stmod.table, add.to.row=addtorow,include.rownames=TRUE, include.colnames=FALSE,caption.placement="top",size="\\footnotesize")

summary(smod_min)
maxatable<-round(summary(mod_max)$coeff[,1:2],digits=2)
minatable<-round(summary(mod_min)$coeff[,1:2],digits=2)
atableAll<-as.data.frame(cbind(minatable,maxatable))
rownames(atableAll)<-c("intercept","structure effect")
#atableAll<-atableAll[,-which(colnames(atableAll)=="X")]

write.csv(atableAll,"../../Analyses/wholegroup_paper1/output/atableAll.csv",row.names=FALSE)

atableAll<-read.csv("../../Analyses/wholegroup_paper1/output/atableAll.csv",header=TRUE)
rownames(atableAll)<-c("intercept","structure effect")

atmod.table<-xtable(atableAll,caption="\\textbf{Summaries of linear mixed-effects models comparing effects of ambient versus structural controls on daily minimum and maximum air temperature in climate change experiments, across the year.} Estimates (est.) are the intercept (representing ambient controls) and coefficient (representing structure effects) from the models; se is the standard error for these estimates. For these annual models, differences between control types were not significant based on Type II Wald $\\chi^{2}$ tests of fixed effects for minimum air temperature ($\\chi^{2}$=1.07, df=1, p=0.30), nor for maximum air temperature ($\\chi^{2}$=0.01, df=1, p=0.91). For both models, random effects of site (n=4) and year nested within site (n=20) were fit with a random slope and intercept structure; total number of observations= 44,085; units are \\degree C.",label="table:shamamb_airtemp",align=c(
  "p{0.13\\textwidth}",
  "|p{0.05\\textwidth}",
  "p{0.05\\textwidth}|",
  "p{0.05\\textwidth}",
  "p{0.05\\textwidth}|"))
addtorow2 <- list()
addtorow2$pos <- list(0, 0)
addtorow2$command <- c("&\\multicolumn{2}{c |}{min air temp.} &\\multicolumn{2}{c |}{max air temp.}\\\\\n \\hline","predictor & est. & se & est. & se\\\\\n")

print(atmod.table, add.to.row=addtorow2, include.rownames=TRUE, include.colnames=FALSE,caption.placement="top",size="\\footnotesize")

summary(mod_max)
#Summary of a linear mixed-effects model comparing effects of ambient versus structural controls on daily soil moisture (\\% volumetric water content, VWC) in climate change experiments across the year.
#soil moisture
expclim_cont$soilmois_perc<-expclim_cont$soilmois1*100
  moismod<-lmer(soilmois_perc~temptreat + (temptreat|site/styear), data=expclim_cont, REML=FALSE)
  
  moistable<-cbind(round(summary(moismod)$coeff[,1:2],digits=2))#
rownames(moistable)<-c("intercept","structure effect")

write.csv(moistable,"../../Analyses/wholegroup_paper1/output/moistable.csv",row.names=FALSE)
summary(moistable)

#monthly models
#soil temps first
  expclim_cont$date<-strptime(paste(expclim_cont$year, expclim_cont$doy), "%Y%j")
  expclim_cont$month<-substring(expclim_cont$date,6,7)
  months<-sort(unique(expclim_cont$month))
  coefs_allyear<-c()
  for (i in 1:length(months)){
    monthdat<-expclim_cont[expclim_cont$month==months[i],]
    monthdat$temptreat <- relevel(as.factor( monthdat$temptreat), ref = "ambient")
    smod_mn<-lmer(soiltemp1_mean~temptreat + (temptreat|site/styear), data=monthdat, REML=FALSE)
    smod_max<-lmer(soiltemp1_max~temptreat + (temptreat|site/styear), data=monthdat, REML=FALSE)
    smod_min<-lmer(soiltemp1_min~temptreat + (temptreat|site/styear), data=monthdat, REML=FALSE)
    coefs<-cbind(c(months[i]," "),c("intercept","structure effect"),round(summary(smod_mn)$coeff[,1:2],digits=2),rbind(Anova(smod_mn),c("","","")),round(summary(smod_min)$coeff[,1:2],digits=4),rbind(Anova(smod_min),c("","","")),round(summary(smod_max)$coeff[,1:2],digits=2),rbind(Anova(smod_max),c("","","")))#
    rownames(coefs)<-NULL
    coefs_allyear<-rbind(coefs_allyear,coefs)
  }
  coefs_allyear<-coefs_allyear[,which(colnames(coefs_allyear)!="Df")]
  colnames(coefs_allyear)[14]<-"p2"
  colnames(coefs_allyear)[10]<-"p1"
  colnames(coefs_allyear)[6]<-"p"
      rownames(coefs_allyear)<-NULL

write.csv(coefs_allyear,"../../Analyses/wholegroup_paper1/output/coefs_allyear.csv",row.names=FALSE)
#air temp monthly mods
    coefs_allyear<-c()
  for (i in 1:length(months)){
    monthdat<-expclim_cont[expclim_cont$month==months[i],]
    monthdat$temptreat <- relevel(as.factor( monthdat$temptreat), ref = "ambient")
    amod_max<-lmer(airtemp_max~temptreat + (temptreat|site/year), data=monthdat, REML=FALSE)
    amod_min<-lmer(airtemp_min~temptreat + (temptreat|site/year), data=monthdat, REML=FALSE)
    coefs<-cbind(c(months[i]," "),c("intercept","structure effect"),round(summary(amod_min)$coeff[,1:2],digits=4),rbind(Anova(amod_min),c("","","")),round(summary(amod_max)$coeff[,1:2],digits=4),rbind(Anova(amod_max),c("","","")))#
    rownames(coefs)<-NULL
    coefs_allyear<-rbind(coefs_allyear,coefs)
  }
  coefs_allyear<-coefs_allyear[,which(colnames(coefs_allyear)!="Df")]
  colnames(coefs_allyear)[10]<-"p1"
  colnames(coefs_allyear)[6]<-"p"
        rownames(coefs_allyear)<-NULL
write.csv(coefs_allyear,"../../Analyses/wholegroup_paper1/output/aircoefs_allyear.csv",row.names=FALSE)
#soil moisture monthly mods
expclim_cont$soilmois_perc<-expclim_cont$soilmois1*100
  coefs_allyear2<-c()
  for (i in 1:length(months)){
    monthdat<-expclim_cont[expclim_cont$month==months[i],]
    monthdat$temptreat<- relevel(as.factor( monthdat$temptreat), ref = "ambient")
    smod<-lmer(soilmois_perc~temptreat + (temptreat|site/year), data=monthdat, REML=FALSE)
    coefs<-cbind(c(months[i]," "),c("intercept","structure effect"),round(summary(smod)$coeff[,1:2],digits=4),rbind(round(Anova(smod),digits=4),c("","","")))#
    rownames(coefs)<-NULL
    coefs_allyear2<-rbind(coefs_allyear2,coefs)
  }
  coefs_allyear2$Chisq<-round(as.numeric(coefs_allyear2$Chisq),digits=2)
  colnames(coefs_allyear2)<-c("mon","predictor","est.","se","chi","df","p")

write.csv(coefs_allyear2,"../../Analyses/wholegroup_paper1/output/moistcoefs_allyear.csv",row.names=FALSE)

moistable<-read.csv("../../Analyses/wholegroup_paper1/output/moistable.csv",header=TRUE)
rownames(moistable)<-c("intercept","structure effect")
 moismod.table<-xtable(moistable,caption="\\textbf{Summary of a linear mixed-effects model comparing effects of ambient versus structural controls on daily soil moisture (\\% volumetric water content, VWC) in climate change experiments across the year.} Estimates (est.) are the intercept (representing ambient controls) and coefficient (representing structure effects) from the models; se is the standard error for these estimates. For this annual model, the difference between control types was significant based on Type II Wald $\\chi^{2}$ tests of fixed effects ($\\chi^{2}$=89.95, df=1, p<0.001). Random effects of site (n=5) and year nested within site (n=21 year-site combinations) were fit with a random slope and intercept structure; total number of observations= 44,468.",label="table:shamamb_soilmois",align=c(
    "p{0.18\\textwidth}|",
    "p{0.16\\textwidth}|",
    "p{0.05\\textwidth}|"))
  addtorow3 <- list()
  addtorow3$pos <- list(0, 0)
  addtorow3$command <- c("&\\multicolumn{2}{c |}{soil moisture (vwc)}\\\\\n \\hline", "predictor & est. & se\\\\\n")
  
  print(moismod.table, add.to.row=addtorow3, include.rownames=TRUE, include.colnames=FALSE,caption.placement="top",size="\\footnotesize")


