## Started April 2021 ##
## By Ailene ##

## plot species estimates by lifeform

############################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = TRUE)

#libraries
library(RColorBrewer)
library(dplyr)
library(brms)
library(scales)
# Setting working directory.


if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/meta_ep2/radcliffe") 
} else setwd("~C:/Users/ailene.ettinger/Documents/GitHub/radcliffe")


figpath <- "Analyses/soilmoisture/figures"


#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=TRUE)
forms<-read.csv("Analyses/output/lifeform.csv", header=TRUE)

#Make some choices about how to restrict the data and which model to use:
remove.conifers=TRUE
use.airtemp=TRUE
use.centmod=FALSE

#standard data wrangling to get expclim2 for climate analyses and expgdd
#for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")

#summarize climate data by plot (annual and seasonal temp, soil mois), 
#merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
source("Analyses/source/climsum_byplot.R")

#Prepare data for phenology model fitting
source("Analyses/source/stanprep_phenmods.R")

##Now something with functional group
source("Analyses/source/get_lifeform.R")

##get ecosystem
source("Analyses/source/get_ecosystem.R")

#create function to plot soil mois vs temp effects and also show interaction at the same time
ploteffs <- function(phenophase, modname){ 
  
  mod<-modname
  fit <- fixef(mod)

  sp<-coef(mod,probs=c(0.1,0.9))$sp
  intercepts<-sp[,,1]
  moisef<-as.data.frame(sp[,,3])
  moisef$spnumbb<-rownames(moisef)

  tempef<-as.data.frame(sp[,,2])
  tempef$spnumbb<-rownames(tempef)

  
  alleff <- merge(moisef, tempef, by="spnumbb", suffixes=c("moist", "temp"))
  if(dim(fit)[1]==4){intef<-as.data.frame(sp[,,4])
    intef$spnumbb<-rownames(intef)
    alleff <- merge(alleff, intef, by="spnumbb")
  }
  colnames(alleff)[which(colnames(alleff)=="Estimatetemp")]<-"Temperature.Effect"
  colnames(alleff)[which(colnames(alleff)=="Estimatemoist")]<-"Soil.Moisture.Effect"
  if(dim(fit)[1]==4){colnames(alleff)[which(colnames(alleff)=="Estimate")]<-"Temp.SM.Interaction"
  ggplot(alleff, aes(x = Temperature.Effect, y = Soil.Moisture.Effect, color=Temp.SM.Interaction)) +
  ggtitle(paste(phenophase)) +
  geom_point(aes(size = Temp.SM.Interaction), alpha = 0.5) +
  scale_colour_viridis_c()}
  if(dim(fit)[1]==3){colnames(alleff)[which(colnames(alleff)=="Estimate")]<-"Temp.SM.Interaction"
  ggplot(alleff, aes(x = Temperature.Effect, y = Soil.Moisture.Effect)) +
    ggtitle(paste(phenophase)) +
    geom_point(aes(size = 12), alpha = 0.5) 
    #scale_colour_viridis_c()
  }
}

#First, budburst
figname<- "bbmodeffplot.pdf"
modfile<-load("Analyses/output/brms/testm5cent.brms.bb.Rda")
pdf(file.path(figpath,figname), height=8,width=10)
ploteffs("budburst",testm5cent.brms)
dev.off()

#Next, leafout
figname<- "lomodeffplot.pdf"
modfile<-load("Analyses/output/brms/testm5cent.brms.lo.Rda")
pdf(file.path(figpath,figname), height=8,width=10)
ploteffs("leafout",testm5cent.lod.brms)
dev.off()

#Last, flowering
figname<- "flmodeffplot.pdf"
modfile<-load("Analyses/output/brms/testm5cent.brms.ff.Rda")
pdf(file.path(figpath,figname), height=8,width=10)
ploteffs("flowering",testm5cent.ffd.brms)
dev.off()

#budburst mod without interactions
figname<- "bbnointmodeffplot.pdf"
modfile<-load("Analyses/output/brms/testmcentnoint.brms.bb.Rda")
pdf(file.path(figpath,figname), height=8,width=10)
ploteffs("budburst (no int)",testm5cent.lod.brms)
dev.off()


####################################
###### old version with base R #####
####################################
figname<-"smtempplot.pdf"
pdf(file.path(figpath,figname), height=8,width=10)

bbmod<- load("Analyses/output/brms/testm5cent.brms.bb.Rda")
mod<- testm5cent.brms
fit <- fixef(mod)

sp<-coef(mod,probs=c(0.1,0.9))$sp
intercepts<-sp[,,1]
moisef<-as.data.frame(sp[,,3])
moisef$spnumbb<-rownames(moisef)

tempef<-as.data.frame(sp[,,2])
tempef$spnumbb<-rownames(tempef)

intef<-as.data.frame(sp[,,4])
intef$spnumbb<-rownames(intef)


## START Lizzie's figure attempt
alleff <- merge(moisef, tempef, by="spnumbb", suffixes=c("moist", "temp"))
alleff <- merge(alleff, intef, by="spnumbb")

library(ggplot2)
pdf(file.path(figpath,"bbmodeffplot.pdf"), height=8,width=10)

ggplot(alleff, aes(x = Estimatetemp, y = Estimatemoist, color=Estimate)) +
    geom_point(aes(size = Estimate), alpha = 0.5) +
    scale_colour_viridis_c()
dev.off()
pdf(file.path(figpath,"bbmodeffplot_scale"), height=8,width=10)

ggplot(alleff, aes(x = Estimatetemp, y = Estimatemoist, color=Estimate)) +
  geom_point(aes(size = Estimate), alpha = 0.5) +
  scale_colour_viridis_c()+
xlim(-30, 10)
dev.off()
## END Lizzie's figure attempt


#quartz(height=4,width=10)
par(mfcol=c(2,3))
plot(tempef$Estimate,moisef$Estimate,main="Budburst",xlab="Temp effects",ylab="Moisture effects", pch=16,col="gray",bty="l")
plot(tempef$Estimate,intef$Estimate,main="Budburst",xlab="Temp effects",ylab="Interaction", pch=16,col="gray",bty="l")

#add leafout
lomod<- load("Analyses/output/brms/testm5cent.brms.lo.Rda")
mod<- testm5cent.lod.brms
fit <- fixef(mod)

sp<-coef(mod,probs=c(0.1,0.9))$sp
intercepts<-sp[,,1]
moisef<-as.data.frame(sp[,,3])
moisef$spnumbb<-rownames(moisef)

tempef<-as.data.frame(sp[,,2])
tempef$spnumbb<-rownames(tempef)

intef<-as.data.frame(sp[,,4])
intef$spnumbb<-rownames(intef)

plot(tempef$Estimate,moisef$Estimate,main="Leafout",xlab="Temp effects",ylab="Moisture effects", pch=16,col="gray",bty="l")
plot(tempef$Estimate,intef$Estimate,main="Leafout",xlab="Temp effects",ylab="Interaction", pch=16,col="gray",bty="l")

#add flowering
flmod<- load("Analyses/output/brms/testm5cent.brms.ff.Rda")
mod<- testm5cent.ffd.brms
fit <- fixef(mod)

sp<-coef(mod,probs=c(0.1,0.9))$sp
intercepts<-sp[,,1]
moisef<-as.data.frame(sp[,,3])
moisef$spnumbb<-rownames(moisef)

tempef<-as.data.frame(sp[,,2])
tempef$spnumbb<-rownames(tempef)

intef<-as.data.frame(sp[,,4])
intef$spnumbb<-rownames(intef)
plot(tempef$Estimate,moisef$Estimate,main="Flowering",xlab="Temp effects",ylab="Moisture effects", pch=16,col="gray",bty="l")
plot(tempef$Estimate,intef$Estimate,main="Flowering",xlab="Temp effects",ylab="Interaction", pch=16,col="gray",bty="l")

dev.off()


## START Lizzie's figure attempt
alleff <- merge(moisef, tempef, by="spnumbb", suffixes=c("moist", "temp"))
alleff <- merge(alleff, intef, by="spnumbb")

library(ggplot2)

ggplot(alleff, aes(x = Estimatetemp, y = Estimatemoist, color=Estimate)) +
    geom_point(aes(size = Estimate), alpha = 0.5) +
    scale_colour_viridis_c()
## END Lizzie's figure attempt
