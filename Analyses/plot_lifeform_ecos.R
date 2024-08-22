## Started April 2021 ##
## By Ailene ##

## plot species estimates by lifeform

############################################
# housekeeping
rm(list=ls()) 

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
use.centmod=TRUE

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
#First, Leaf-Out
figname<-"histloform.pdf"

lomod<- load("Analyses/output/brms/testm5cent.brms.lo.Rda")
mod<- testm5cent.lod.brms

fit <- fixef(mod)
#getest.bb.sp <- function(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm, wet2sm,spnum,sitenum){
listofdraws <-as.data.frame(as.matrix(mod))
matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="tree"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
treem<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#check: 
#colnames(treem)
#mois
matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="shrub"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="forb"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forbm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="grass"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grassm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#temp
matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="tree"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
treet<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="shrub"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubt<-as.matrix(listofdraws%>% select(all_of(cols2use)))
#mean(shrubt)
#quantile(shrubt,probs=c(0.1,.9))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="forb"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forbt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="grass"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grasst<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#temp*mois interaction
matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="tree"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
treeint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="shrub"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="forb"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forbint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="grass"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grassint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

pdf(file.path(figpath,figname), height=5,width=12)
#quartz(height=5,width=12)
#windows()
par(mfcol=c(1,3))
hist(forbt, main="Temp",xlim=c(-60,60),col=alpha("darkgreen",.4),xlab="Effect on leafout per SD")
hist(treet, col=alpha("darkred",.4), add=TRUE)
hist(shrubt, col=alpha("darkblue",.4), add=TRUE)
hist(grasst, col=alpha("goldenrod",.4), add=TRUE)
hist(forbm, main="Mois",xlim=c(-30,30),col=alpha("darkgreen",.4),xlab="Effect on leafout per SD")
hist(treem, col=alpha("darkred",.5), add=TRUE)
hist(shrubm,col=alpha("darkblue",.5),add=TRUE)
hist(grassm,col=alpha("goldenrod",.5), add=TRUE)
hist(forbint, main="Temp*Mois",xlim=c(-30,30),col=alpha("darkgreen",.4),xlab="Effect on leafout per SD")
hist(treeint, xlim=c(-30,30),col=alpha("darkred",.5), add=TRUE)
hist(shrubint, xlim=c(-30,30),col=alpha("darkblue",.5),add=TRUE)
hist(grassint, xlim=c(-30,30),col=alpha("goldenrod",.5), add=TRUE)

dev.off()
#now as s density curve instead

figname<-"curveloform.pdf"
pdf(file.path(figpath,figname), height=5,width=12)
#quartz(height=10,width=4)
#windows()
par(mfcol=c(1,3))
dta_tree <- density(treet, na.rm = TRUE)
dta_shrub <- density(shrubt, na.rm = TRUE)
dta_forb <- density(forbt, na.rm = TRUE)
dta_grass <- density(grasst, na.rm = TRUE)

plot(dta_forb, col ="darkgreen", main = "Temperature", 
    ylim = c(0, max(dta_forb$y,dta_tree$y,dta_shrub$y,dta_grass$y)),
    lwd=2,bty="l", xlab="Effect on leafout per SD")
lines(dta_tree, col = "darkred",lwd=2)
lines(dta_shrub, col = "darkblue",lwd=2)
lines(dta_grass, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

legend("topright", c("Trees","Shrubs","Forbs","Grass"), lty = c(1,1), col = c("darkgreen","darkblue","darkred",'goldenrod'), lwd=2,bty="n")

dta_treem <- density(treem, na.rm = TRUE)
dta_shrubm <- density(shrubm, na.rm = TRUE)
dta_forbm <- density(forbm, na.rm = TRUE)
dta_grassm <- density(grassm, na.rm = TRUE)

plot(dta_forbm, col ="darkgreen", main = "Moisture", 
     ylim = c(0, max(dta_forbm$y,dta_treem$y,dta_shrubm$y,dta_grassm$y)),
     lwd=2,bty="l", xlab="Effect on leafout per SD")
lines(dta_treem, col = "darkred",lwd=2)
lines(dta_shrubm, col = "darkblue",lwd=2)
lines(dta_grassm, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

dta_treeint <- density(treeint, na.rm = TRUE)
dta_shrubint <- density(shrubint, na.rm = TRUE)
dta_forbint <- density(forbint, na.rm = TRUE)
dta_grassint<- density(grassint, na.rm = TRUE)

plot(dta_forbint, col ="darkgreen", main = "Temp*Moist", 
     ylim = c(0, max(dta_forbint$y,dta_treeint$y,dta_shrubint$y,dta_grassint$y)),
     lwd=2,bty="l", xlab="Effect on leafout per SD")
lines(dta_treeint, col = "darkred",lwd=2)
lines(dta_shrubint, col = "darkblue",lwd=2)
lines(dta_grassint, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

dev.off()


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
#windows()
par(mfrow=c(2,3))
hist(tempform$Estimate[tempform$form=="forb"],main="Temperature Effect",xlim=c(-35,35),ylim=c(0,20),col=alpha("darkgreen",.5),xlab="Change in temperature per SD")
hist(tempform$Estimate[tempform$form=="tree"],col=alpha("darkred",.7),add=TRUE)
hist(tempform$Estimate[tempform$form=="shrub"], col=alpha("blue",.5), add = TRUE)
hist(tempform$Estimate[tempform$form=="grass"], main="Grass",col=alpha("goldenrod",.7), add=TRUE)
#abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
legend("topright",legend=c("trees","shrubs","herbs","grasses"),fill=alpha(c("darkred","blue","darkgreen","goldenrod"),.7))
abline(v=0, lwd=1,col="black",lty=2)

hist(moisform$Estimate[moisform$form=="forb"],main="Moisture Effect",xlim=c(-7,7),ylim=c(0,20),col=alpha("darkgreen",.5),xlab="Change in VWC per SD")
hist(moisform$Estimate[moisform$form=="tree"],col=alpha("darkred",.5),add=TRUE)
hist(moisform$Estimate[moisform$form=="shrub"], col=alpha("blue",.5), add = TRUE)
hist(moisform$Estimate[moisform$form=="grass"], main="Grass",col=alpha("goldenrod",.7), add=TRUE)
#abline(v=fixef(mod)[3,1], lwd=2,col="red",lty=1)
abline(v=0, lwd=1,col="black",lty=2)

hist(intform$Estimate[intform$form=="forb"],main="Temp*Mois",xlim=c(-15,15),ylim=c(0,20),col=alpha("darkgreen",.5),xlab="Temp effects")
hist(intform$Estimate[intform$form=="tree"],col=alpha("darkred",.5),add=TRUE)
hist(intform$Estimate[intform$form=="shrub"], col=alpha("blue",.5), add = TRUE)
hist(intform$Estimate[intform$form=="grass"], main="Grass",col=alpha("goldenrod",.9), add=TRUE)
#abline(v=fixef(mod)[4,1], lwd=2,col="red",lty=1)
abline(v=0, lwd=1,col="black",lty=2)

dev.off()

#Second, Budburst
figname<-"hist.bbform.pdf"

bbmod<- load("Analyses/output/brms/testm5cent.brms.bb.Rda")
mod<- testm5cent.brms
fit <- fixef(mod)

#getest.bb.sp <- function(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm, wet2sm,spnum,sitenum){
listofdraws <-as.data.frame(as.matrix(mod))
matchExpression <- paste("r_sp[",bbform$spnumbb[bbform$form=="tree"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
treem<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#check: 
#colnames(treem)
#mois
matchExpression <- paste("r_sp[",bbform$spnumbb[bbform$form=="shrub"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubm<-as.matrix(listofdraws%>% select(all_of(cols2use)))
quantile(shrubm,.5)
mean(shrubm)
#temp
matchExpression <- paste("r_sp[",bbform$spnumbb[bbform$form=="tree"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
treet<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",bbform$spnumbb[bbform$form=="shrub"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubt<-as.matrix(listofdraws%>% select(all_of(cols2use)))
#mean(shrubt)
#quantile(shrubt,probs=c(0.1,.9))

#temp*mois interaction
matchExpression <- paste("r_sp[",bbform$spnumbb[bbform$form=="tree"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
treeint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",bbform$spnumbb[bbform$form=="shrub"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

pdf(file.path(figpath,figname), height=5,width=12)
#quartz(height=5,width=12)
#windows()
par(mfcol=c(1,3))
hist(treet,main="Temp",xlim=c(-60,60),col=alpha("darkred",.4),xlab="Effect on budburst per SD")
hist(shrubt, col=alpha("darkblue",.4), add=TRUE)
hist(treem, main="Mois",xlim=c(-30,30),col=alpha("darkred",.4),xlab="Effect on budburst per SD")
hist(shrubm,col=alpha("darkblue",.5),add=TRUE)
hist(treeint, main="Temp*Mois",xlim=c(-30,30),col=alpha("darkred",.4),xlab="Effect on budburst per SD")
hist(shrubint, xlim=c(-30,30),col=alpha("darkblue",.5),add=TRUE)

dev.off()
#now as s density curve instead

figname<-"curvebbform.pdf"
pdf(file.path(figpath,figname), height=5,width=12)
#quartz(height=10,width=4)
#windows()
par(mfcol=c(1,3))
dta_tree <- density(treet, na.rm = TRUE)
dta_shrub <- density(shrubt, na.rm = TRUE)

plot(dta_tree, col ="darkred", main = "Temperature", 
     ylim = c(0, max(dta_tree$y,dta_shrub$y)),
     lwd=2,bty="l", xlab="Effect on leafout per SD")
lines(dta_shrub, col = "darkblue",lwd=2)
#abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

legend("topright", c("Trees","Shrubs"), lty = c(1,1), col = c("darkred","darkblue"), lwd=2,bty="n")

dta_treem <- density(treem, na.rm = TRUE)
dta_shrubm <- density(shrubm, na.rm = TRUE)

plot(dta_treem, col ="darkred", main = "Moisture", 
     ylim = c(0, max(dta_treem$y,dta_shrubm$y)),
     lwd=2,bty="l", xlab="Effect on leafout per SD")
lines(dta_shrubm, col = "darkblue",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

dta_treeint <- density(treeint, na.rm = TRUE)
dta_shrubint <- density(shrubint, na.rm = TRUE)

plot(dta_treeint, col ="darkred", main = "Temp*Moist", 
     ylim = c(0, max(dta_treeint$y,dta_shrubint$y)),
     lwd=2,bty="l", xlab="Effect on leafout per SD")
lines(dta_shrubint, col = "darkblue",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

dev.off()



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
#abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
legend("topright",legend=c("trees","shrubs"),fill=alpha(c("darkred","blue"),.7))
abline(v=0, lwd=1,col="black",lty=2)

hist(moisform$Estimate[moisform$form=="tree"],main="Moisture Effect",xlim=c(-7,7),ylim=c(0,20),col=alpha("darkred",.5),xlab="Temp effects")
hist(moisform$Estimate[moisform$form=="shrub"], col=alpha("blue",.5), add = TRUE)
#abline(v=fixef(mod)[3,1], lwd=2,col="red",lty=1)
abline(v=0, lwd=1,col="black",lty=2)

hist(intform$Estimate[intform$form=="tree"],main="Temp*Mois",xlim=c(-15,15),ylim=c(0,20),col=alpha("darkred",.5),xlab="Temp effects")
hist(intform$Estimate[intform$form=="shrub"], col=alpha("blue",.5), add = TRUE)
#abline(v=fixef(mod)[4,1], lwd=2,col="red",lty=1)
abline(v=0, lwd=1,col="black",lty=2)

dev.off()



#Third,Flowering
figname<-"hist.ffform.pdf"

flmod<- load("Analyses/output/brms/testm5cent.brms.ff.Rda")
mod<- testm5cent.ffd.brms
fit <- fixef(mod)
listofdraws <-as.data.frame(as.matrix(mod))
matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="shrub"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
treem<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#check: 
#colnames(treem)
#mois
matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="shrub"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubm<-as.matrix(listofdraws%>% select(all_of(cols2use)))


matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="forb"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forbm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="grass"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grassm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#temp
matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="shrub"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="forb"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forbt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="grass"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grasst<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#temp*mois interaction
matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="shrub"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="forb"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forbint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="grass"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grassint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

pdf(file.path(figpath,figname), height=4,width=12)
#quartz(height=4,width=12)
par(mfcol=c(3,2))
hist(forbt,main="Temp",xlim=c(-30,30),col=alpha("darkgreen",.4),xlab="Effect on budburst per SD")
hist(grasst, xlim=c(-30,30),col=alpha("goldenrod",.5), add=TRUE)
hist(shrubt, xlim=c(-30,30),col=alpha("darkblue",.5),add=TRUE)

hist(forbm, main="Mois",xlim=c(-30,30),col=alpha("darkgreen",.4),xlab="Effect on budburst per SD")
hist(grassm,col=alpha("goldenrod",.5),add=TRUE)
hist(shrubm,col=alpha("darkblue",.5),add=TRUE)

hist(forbint, main="Temp*Mois",xlim=c(-30,30),col=alpha("darkgreen",.4),xlab="Effect on budburst per SD")
hist(shrubint, xlim=c(-25,25),col=alpha("darkblue",.5),add=TRUE)
hist(grassint, xlim=c(-25,25),col=alpha("goldenrod",.5), add=TRUE)
     
dev.off()
mean(shrubt);mean(forbt);mean(grasst)
mean(shrubm);mean(forbm);mean(grassm)


#Fourth,fruiting
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

#####################################
####### Ecosystem differences #######
#####################################
#start by using species random effects not posteriors
lomod<- load("Analyses/output/brms/testm5cent.brms.lo.Rda")
mod<- testm5cent.lod.brms
fit <- fixef(mod)
#getest.bb.sp <- function(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm, wet2sm,spnum,sitenum){
listofdraws <-as.data.frame(as.matrix(mod))
matchExpression <- paste(loecos$spnum[loecos$ecosystem =="forest"],",mois]", collapse = "|",sep="")
forestm<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(loecos$spnum[loecos$ecosystem =="grassland"],",mois]", collapse = "|",sep="")
grassm<-as.matrix(listofdraws%>% select(matches(matchExpression)))

#temp
matchExpression <- paste(loecos$spnum[loecos$ecosystem =="forest"],",temp]", collapse = "|",sep="")
forestt<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(loecos$spnum[loecos$ecosystem =="grassland"],",temp]", collapse = "|",sep="")
grasst<-as.matrix(listofdraws%>% select(matches(matchExpression)))
meanz <- unlist(lapply(forestt, mean))
dim(forestt)
hist(colMeans(forestt))
mean(forestt)
mean(grasst)
quantile(forestt,probs=c(0.1,.9))
quantile(grasst,probs=c(0.1,.9))

figname<-"histloecos.pdf"

pdf(file.path(figpath,figname), height=12,width=8)
#quartz(height=10,width=4)
#windows()
par(mfcol=c(2,2))
hist(forestt, main="Forest",xlim=c(-30,30),col=alpha("darkgreen",.5),xlab="Temp effects")
hist(grasst, main="Grassland",xlim=c(-30,30),col=alpha("goldenrod",.5),xlab="Temp effects")
hist(forestm, main="Forest",xlim=c(-30,30),col=alpha("darkgreen",.5),xlab="Mois effects")
hist(grassm, main="Grassland",xlim=c(-30,30),col=alpha("goldenrod",.5),xlab="Mois effects")
dev.off()

#same figure but not using posterior samples- just species random effects, as I'm not sure that the above is working
sp<-coef(mod,probs=c(0.1,0.9))$sp
intercepts<-sp[,,1]
moisef<-as.data.frame(sp[,,3])
moisef$spnum<-rownames(moisef)

tempef<-as.data.frame(sp[,,2])
tempef$spnum<-rownames(tempef)

intef<-as.data.frame(sp[,,4])
intef$spnum<-rownames(intef)

loecos$spnum<-as.character(loecos$spnum)

tempecos<-left_join(tempef,loecos, by="spnum")
tempecos$sp.eco<-paste(tempecos$sp.name,tempecos$ecosystem)
tempecos2<- tempecos %>% # start with the data frame
  distinct(sp.eco, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,ecosystem,Estimate)

moisecos<-left_join(moisef,loecos)
moisecos$sp.eco<-paste(moisecos$sp.name,moisecos$ecosystem)
moisecos2<- moisecos %>% # start with the data frame
  distinct(sp.eco, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,ecosystem,Estimate)

intecos<-left_join(intef,loecos)
intecos$sp.eco<-paste(intecos$sp.name,intecos$ecosystem)
intecos2<- intecos %>% # start with the data frame
  distinct(sp.eco, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,ecosystem,Estimate)

figname<-"histloecos_spef.pdf"

#windows(height=4,width=10)
par(mfcol=c(1,2))
hist(tempecos2$Estimate[tempecos2$ecosystem=="forest"],main="Temperature Effect",xlim=c(-40,35),col=alpha("darkgreen",.5),xlab="Temp effects")
hist(tempecos2$Estimate[tempecos2$ecosystem=="grassland"], col=alpha("goldenrod",.5), add = TRUE)
abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
legend("topright",legend=c("forest","grassland"),fill=alpha(c("darkgreen","goldenrod"),.7))
abline(v=0, lwd=1,col="black",lty=2)

hist(moisecos2$Estimate[moisecos2$ecosystem=="forest"],main="Moisture Effect",xlim=c(-40,35),col=alpha("darkgreen",.5),xlab="Temp effects")
hist(moisecos2$Estimate[moisecos2$ecosystem=="grassland"], col=alpha("goldenrod",.5), add = TRUE)
abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

hist(intform$Estimate[intform$form=="tree"],main="Temp*Mois",xlim=c(-15,15),ylim=c(0,20),col=alpha("darkred",.5),xlab="Temp effects")
hist(intform$Estimate[intform$form=="shrub"], col=alpha("blue",.5), add = TRUE)
abline(v=fixef(mod)[4,1], lwd=2,col="red",lty=1)
abline(v=0, lwd=1,col="black",lty=2)

dev.off()
