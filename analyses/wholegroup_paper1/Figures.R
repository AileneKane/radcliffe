# ------------------------------------------
# Mock figures showing spatial variation in experimental warming
# A. Ettinger, aettinger@fas.harvard.edu
# Description: Mock plot of spatial variation: blocks vs plots vs treatment levels

setwd("~/git/radcliffe")
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Load packages
library(lme4)

# Read in experimental climate data
expclim<-read.csv("analyses/expclim.csv", header=T)
expclim_control<-expclim[expclim$temptreat=="0", ]#these are sham/true controls
expclim_control$temptreat<-factor(expclim_control$temptreat)
expclim_control$site<-as.factor(expclim_control$site)
expclim_control$block<-as.factor(expclim_control$block)
expclim_control$plot<-as.factor(expclim_control$plot)
boxplot(expclim_control$airtemp_min~expclim_control$plot)
##just look at sites with blocks AND plots
expclim_block<-expclim[which(!is.na(expclim$block)),]
expclim_block<-expclim_block[-which(expclim$temptreat=="ambient"),]

mod<-lmer(soiltemp1_mean~temptreat+(1|site/plot), data=expclim)
# Fit mixed model with just plot and block- temperature in control plots?
summary(mod)

boxplot(expclim$soiltemp1_mean~expclim$temptreat)
boxplot(expclim$soiltemp1_mean~expclim$plot)

