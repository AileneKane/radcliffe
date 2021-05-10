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
setwd("~/Documents/GitHub/radcliffe")

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

#Prepare data for phenology models in stan
source("Analyses/source/stanprep_phenmods.R")

##Now something with functional group- do either lo or fl for this?
source("Analyses/source/get_lifeform.R")

#First, Leaf-Out
figname<-"histloform.pdf"
#figname<-"histbbloform.pdf"

lomod<- load("Analyses/output/brms/testm5cent.brms.lo.Rda")
mod<- testm5cent.lod.brms
fit <- fixef(mod)
#getest.bb.sp <- function(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm, wet2sm,spnum,sitenum){
listofdraws <-as.data.frame(as.matrix(mod))
matchExpression <- paste(loform$spnumlo[loform$form=="tree"],",mois]", collapse = "|",sep="")
treem<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(loform$spnumlo[loform$form=="shrub"],",mois]", collapse = "|",sep="")
shrubm<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(loform$spnumlo[loform$form=="forb"],",mois]", collapse = "|",sep="")
forbm<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(loform$spnumlo[loform$form=="grass"],",mois]", collapse = "|",sep="")
grassm<-as.matrix(listofdraws%>% select(matches(matchExpression)))

#temp
matchExpression <- paste(loform$spnumlo[loform$form=="tree"],",temp]", collapse = "|",sep="")
treet<-as.matrix(listofdraws%>% select(matches(matchExpression)))
meanz <- unlist(lapply(treet, mean))
dim(treet)
hist(colMeans(treet))
quantile(treet,probs=c(0.1,.9))
matchExpression <- paste(loform$spnumlo[loform$form=="shrub"],",temp]", collapse = "|",sep="")
shrubt<-as.matrix(listofdraws%>% select(matches(matchExpression)))
mean(shrubt)
quantile(shrubt,probs=c(0.1,.9))

matchExpression <- paste(loform$spnumlo[loform$form=="forb"],",temp]", collapse = "|",sep="")
forbt<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(loform$spnumlo[loform$form=="grass"],",temp]", collapse = "|",sep="")
grasst<-as.matrix(listofdraws%>% select(matches(matchExpression)))

pdf(file.path(figpath,figname), height=12,width=8)
#quartz(height=10,width=4)
par(mfcol=c(4,2))
hist(treet, main="Trees",xlim=c(-30,30),col=alpha("darkred",.5),xlab="Temp effects")
hist(shrubt, main="Shrubs",xlim=c(-30,30),col=alpha("darkred",.5),xlab="Temp effects")
hist(forbt, main="Herbs",xlim=c(-30,30),col=alpha("darkred",.5),xlab="Temp effects")
hist(grasst, main="Grass",xlim=c(-30,30),col=alpha("darkred",.5),xlab="Temp effects")
hist(treem, main="Trees",xlim=c(-30,30),col=alpha("darkblue",.5),xlab="Mois effects")
hist(shrubm, main="Shrubs",xlim=c(-30,30),col=alpha("darkblue",.5),xlab="Mois effects")
hist(forbm, main="Herbs",xlim=c(-30,30),col=alpha("darkblue",.5),xlab="Mois effects")
hist(grassm, main="Grass",xlim=c(-30,30),col=alpha("darkblue",.5),xlab="Mois effects")

dev.off()
mean(treet);mean(shrubt);mean(forbt);mean(grasst)
mean(treem);mean(shrubm);mean(forbm);mean(grassm)


#same figure but not using posterior samples- just species random effects, as I'm not sure that the above is working
sp<-coef(mod,probs=c(0.1,0.9))$sp
intercepts<-sp[,,1]
moisef<-as.data.frame(sp[,,3])
moisef$spnumlo<-rownames(moisef)

tempef<-as.data.frame(sp[,,2])
tempef$spnumlo<-rownames(tempef)

intef<-as.data.frame(sp[,,4])
intef$spnumlo<-rownames(intef)

#getest.bb.sp <- function(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm, wet2sm,spnum,sitenum){
loform$spnumlo<-as.character(loform$spnumlo)

tempform<-left_join(tempef,loform)
moisform<-left_join(moisef,loform)
intform<-left_join(intef,loform)
figname<-"histbbloform_spef.pdf"
pdf(file.path(figpath,figname), height=8,width=10)
#quartz(height=4,width=10)
par(mfrow=c(2,3))
hist(tempform$Estimate[tempform$form=="forb"],main="Temperature Effect",xlim=c(-35,35),ylim=c(0,20),col=alpha("darkgreen",.5),xlab="Temp effects")
hist(tempform$Estimate[tempform$form=="tree"],col=alpha("darkred",.7),add=TRUE)
hist(tempform$Estimate[tempform$form=="shrub"], col=alpha("blue",.5), add = TRUE)
hist(tempform$Estimate[tempform$form=="grass"], main="Grass",col=alpha("goldenrod",.7), add=TRUE)
abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
legend("topright",legend=c("trees","shrubs","herbs","grasses"),fill=alpha(c("darkred","blue","darkgreen","goldenrod"),.7))
abline(v=0, lwd=1,col="black",lty=2)

hist(moisform$Estimate[moisform$form=="forb"],main="Moisture Effect",xlim=c(-7,7),ylim=c(0,20),col=alpha("darkgreen",.5),xlab="Temp effects")
hist(moisform$Estimate[moisform$form=="tree"],col=alpha("darkred",.5),add=TRUE)
hist(moisform$Estimate[moisform$form=="shrub"], col=alpha("blue",.5), add = TRUE)
hist(moisform$Estimate[moisform$form=="grass"], main="Grass",col=alpha("goldenrod",.7), add=TRUE)
abline(v=fixef(mod)[3,1], lwd=2,col="red",lty=1)
abline(v=0, lwd=1,col="black",lty=2)

hist(intform$Estimate[intform$form=="forb"],main="Temp*Mois",xlim=c(-15,15),ylim=c(0,20),col=alpha("darkgreen",.5),xlab="Temp effects")
hist(intform$Estimate[intform$form=="tree"],col=alpha("darkred",.5),add=TRUE)
hist(intform$Estimate[intform$form=="shrub"], col=alpha("blue",.5), add = TRUE)
hist(intform$Estimate[intform$form=="grass"], main="Grass",col=alpha("goldenrod",.9), add=TRUE)
abline(v=fixef(mod)[4,1], lwd=2,col="red",lty=1)
abline(v=0, lwd=1,col="black",lty=2)

dev.off()

#Second, Budburst
figname<-"hist.bbform.pdf"

bbmod<- load("Analyses/output/brms/testm5cent.brms.bb.Rda")
mod<- testm5cent.brms
fit <- fixef(mod)
#getest.bb.sp <- function(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm, wet2sm,spnum,sitenum){
listofdraws <-as.data.frame(as.matrix(mod))
matchExpression <- paste(bbform$spnumbb[bbform$form=="tree"],",mois]", collapse = "|",sep="")
treem<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(bbform$spnumbb[bbform$form=="shrub"],",mois]", collapse = "|",sep="")
shrubm<-as.matrix(listofdraws%>% select(matches(matchExpression)))

#temp
matchExpression <- paste(bbform$spnumbb[bbform$form=="tree"],",temp]", collapse = "|",sep="")
treet<-as.matrix(listofdraws%>% select(matches(matchExpression)))
mean(treet)
quantile(treet,probs=c(0.1,.9))
matchExpression <- paste(bbform$spnumbb[bbform$form=="shrub"],",temp]", collapse = "|",sep="")
shrubt<-as.matrix(listofdraws%>% select(matches(matchExpression)))
mean(shrubt)
quantile(shrubt,probs=c(0.1,.9))

pdf(file.path(figpath,figname), height=6,width=8)
#quartz(height=10,width=4)
par(mfcol=c(2,2))
hist(treet, main="Trees",xlim=c(-30,30),col=alpha("darkred",.5),xlab="Temp effects")
hist(shrubt, main="Shrubs",xlim=c(-30,30),col=alpha("darkred",.5),xlab="Temp effects")
hist(treem, main="Trees",xlim=c(-30,30),col=alpha("darkblue",.5),xlab="Mois effects")
hist(shrubm, main="Shrubs",xlim=c(-30,30),col=alpha("darkblue",.5),xlab="Mois effects")
dev.off()
mean(treet);mean(shrubt)
mean(treem);mean(shrubm)


#same figure but not using posterior samples- just species random effects, as I'm not sure that the above is working
sp<-coef(mod,probs=c(0.1,0.9))$sp
intercepts<-sp[,,1]
moisef<-as.data.frame(sp[,,3])
moisef$spnumbb<-rownames(moisef)

tempef<-as.data.frame(sp[,,2])
tempef$spnumbb<-rownames(tempef)

intef<-as.data.frame(sp[,,4])
intef$spnumbb<-rownames(intef)

#getest.bb.sp <- function(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm, wet2sm,spnum,sitenum){
bbform$spnumbb<-as.character(bbform$spnumbb)

tempform<-left_join(tempef,bbform)
moisform<-left_join(moisef,bbform)
intform<-left_join(intef,bbform)
figname<-"histbbform_spef.pdf"
pdf(file.path(figpath,figname), height=4,width=10)
#quartz(height=4,width=10)
par(mfcol=c(1,3))
hist(tempform$Estimate[tempform$form=="tree"],main="Temperature Effect",xlim=c(-60,35),ylim=c(0,20),col=alpha("darkred",.5),xlab="Temp effects")
hist(tempform$Estimate[tempform$form=="shrub"], col=alpha("blue",.5), add = TRUE)
abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
legend("topright",legend=c("trees","shrubs"),fill=alpha(c("darkred","blue"),.7))
abline(v=0, lwd=1,col="black",lty=2)

hist(moisform$Estimate[moisform$form=="tree"],main="Moisture Effect",xlim=c(-7,7),ylim=c(0,20),col=alpha("darkred",.5),xlab="Temp effects")
hist(moisform$Estimate[moisform$form=="shrub"], col=alpha("blue",.5), add = TRUE)
abline(v=fixef(mod)[3,1], lwd=2,col="red",lty=1)
abline(v=0, lwd=1,col="black",lty=2)

hist(intform$Estimate[intform$form=="tree"],main="Temp*Mois",xlim=c(-15,15),ylim=c(0,20),col=alpha("darkred",.5),xlab="Temp effects")
hist(intform$Estimate[intform$form=="shrub"], col=alpha("blue",.5), add = TRUE)
abline(v=fixef(mod)[4,1], lwd=2,col="red",lty=1)
abline(v=0, lwd=1,col="black",lty=2)

dev.off()



#Third,Flowering
figname<-"hist.ffform.pdf"

flmod<- load("Analyses/output/brms/testm5cent.brms.ff.Rda")
mod<- testm5cent.ffd.brms
fit <- fixef(mod)
listofdraws <-as.data.frame(as.matrix(mod))

matchExpression <- paste(flform$spnumfl[flform$form=="shrub"],",mois]", collapse = "|",sep="")
shrubm<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(flform$spnumfl[flform$form=="forb"],",mois]", collapse = "|",sep="")
forbm<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(flform$spnumfl[flform$form=="grass"],",mois]", collapse = "|",sep="")
grassm<-as.matrix(listofdraws%>% select(matches(matchExpression)))

#temp
matchExpression <- paste(flform$spnumfl[flform$form=="shrub"],",temp]", collapse = "|",sep="")
shrubt<-as.matrix(listofdraws%>% select(matches(matchExpression)))
mean(shrubt)
quantile(shrubt,probs=c(0.1,.9))

matchExpression <- paste(flform$spnumfl[flform$form=="forb"],",temp]", collapse = "|",sep="")
forbt<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(flform$spnumfl[flform$form=="grass"],",temp]", collapse = "|",sep="")
grasst<-as.matrix(listofdraws%>% select(matches(matchExpression)))

pdf(file.path(figpath,figname), height=12,width=8)
#quartz(height=10,width=4)
par(mfcol=c(3,2))
hist(shrubt, main="Shrubs",xlim=c(-30,30),col=alpha("darkred",.5),xlab="Temp effects")
hist(forbt, main="Herbs",xlim=c(-30,30),col=alpha("darkred",.5),xlab="Temp effects")
hist(grasst, main="Grass",xlim=c(-30,30),col=alpha("darkred",.5),xlab="Temp effects")
hist(shrubm, main="Shrubs",xlim=c(-30,30),col=alpha("darkblue",.5),xlab="Mois effects")
hist(forbm, main="Herbs",xlim=c(-30,30),col=alpha("darkblue",.5),xlab="Mois effects")
hist(grassm, main="Grass",xlim=c(-30,30),col=alpha("darkblue",.5),xlab="Mois effects")
dev.off()
mean(shrubt);mean(forbt);mean(grasst)
mean(shrubm);mean(forbm);mean(grassm)

#Third,fruiting
figname<-"hist.frform.pdf"

frmod<- load("Analyses/output/brms/testm5cent.brms.frd.Rda")
mod<-testm5cent.ffrd.brms
fit <- fixef(mod)
#getest.bb.sp <- function(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm, wet2sm,spnum,sitenum){
listofdraws <-as.data.frame(as.matrix(mod))

matchExpression <- paste(frform$spnumfr[frform$form=="shrub"],",mois]", collapse = "|",sep="")
shrubm<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(frform$spnumfr[frform$form=="forb"],",mois]", collapse = "|",sep="")
forbm<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(frform$spnumfr[frform$form=="grass"],",mois]", collapse = "|",sep="")
grassm<-as.matrix(listofdraws%>% select(matches(matchExpression)))

#temp
matchExpression <- paste(frform$spnumfr[frform$form=="shrub"],",temp]", collapse = "|",sep="")
shrubt<-as.matrix(listofdraws%>% select(matches(matchExpression)))
mean(shrubt)
quantile(shrubt,probs=c(0.1,.9))

matchExpression <- paste(frform$spnumfr[frform$form=="forb"],",temp]", collapse = "|",sep="")
forbt<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(frform$spnumfr[frform$form=="grass"],",temp]", collapse = "|",sep="")
grasst<-as.matrix(listofdraws%>% select(matches(matchExpression)))

pdf(file.path(figpath,figname), height=10,width=8)
#quartz(height=10,width=4)
par(mfcol=c(3,2))
hist(shrubt, main="Shrubs",xlim=c(-30,30),col=alpha("darkred",.5),xlab="Temp effects")
hist(forbt, main="Herbs",xlim=c(-30,30),col=alpha("darkred",.5),xlab="Temp effects")
hist(grasst, main="Grass",xlim=c(-30,30),col=alpha("darkred",.5),xlab="Temp effects")
hist(shrubm, main="Shrubs",xlim=c(-30,30),col=alpha("darkblue",.5),xlab="Mois effects")
hist(forbm, main="Herbs",xlim=c(-30,30),col=alpha("darkblue",.5),xlab="Mois effects")
hist(grassm, main="Grass",xlim=c(-30,30),col=alpha("darkblue",.5),xlab="Mois effects")
dev.off()
mean(shrubt);mean(forbt);mean(grasst)
mean(shrubm);mean(forbm);mean(grassm)

