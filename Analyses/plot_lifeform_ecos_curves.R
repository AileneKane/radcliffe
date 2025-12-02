## Started April 2021 ##
## By Ailene ##

## plot species estimates by lifeform and ecosystem

############################################
# housekeeping
rm(list=ls()) 

#libraries
library(RColorBrewer)
library(dplyr)
library(brms)
library(scales)
# Setting working directory.
#setwd("~/Documents/GitHub/radcliffe")
setwd("C:/Users/ailene.ettinger/Documents/GitHub/radcliffe")

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

##functions to make the plots

#plot.lf<-function(mod,lifeform,)

  
#plot posterior density curves by life form

figname<-"curvebbloflform.pdf"
pdf(file.path(figpath,figname), height=12,width=18)
#quartz(height=12,width=18)
#windows(height=12,width=18)
par(mfrow=c(3,3), mar=c(5,10,4,2)+0.1)

#Budburst

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

#quartz(height=10,width=4)
#windows()
dta_tree <- density(treet, na.rm = TRUE)
dta_shrub <- density(shrubt, na.rm = TRUE)

plot(dta_tree, col ="darkred", main = "Temperature", 
     xlim=c(-40,40),
     ylim = c(0, max(dta_tree$y,dta_shrub$y)),
     lwd=2,bty="l", xlab="Effect on budburst per SD",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
lines(dta_shrub, col = "darkblue",lwd=2)
#abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

legend("topright", c("Trees","Shrubs","Forbs","Grass"), 
       lty = c(1,1), col = c("darkred","darkblue","darkgreen",'goldenrod'),
       lwd=2,bty="n", cex=1.5)
mtext("Budburst",side=2,line=6, cex=1.2)

dta_treem <- density(treem, na.rm = TRUE)
dta_shrubm <- density(shrubm, na.rm = TRUE)

plot(dta_treem, col ="darkred", main = "Moisture", 
     xlim=c(-10,10),
     ylim = c(0, max(dta_treem$y,dta_shrubm$y)),
     lwd=2,bty="l", xlab="Effect on budburst per SD",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
lines(dta_shrubm, col = "darkblue",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

dta_treeint <- density(treeint, na.rm = TRUE)
dta_shrubint <- density(shrubint, na.rm = TRUE)

plot(dta_treeint, col ="darkred", main = "Temp*Moist", 
     xlim=c(-10,10),
     ylim = c(0, max(dta_treeint$y,dta_shrubint$y)),
     lwd=2,bty="l", xlab="Effect on budburst per SD",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
lines(dta_shrubint, col = "darkblue",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

#dev.off()



#Leaf-Out
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

dta_tree <- density(treet, na.rm = TRUE)
dta_shrub <- density(shrubt, na.rm = TRUE)
dta_forb <- density(forbt, na.rm = TRUE)
dta_grass <- density(grasst, na.rm = TRUE)

plot(dta_forb, col ="darkgreen", main = " ", 
     xlim=c(-40,40),
    ylim = c(0, max(dta_forb$y,dta_tree$y,dta_shrub$y,dta_grass$y)),
    lwd=2,bty="l", xlab="Effect on leafout per SD",
    cex.axis=1.5, cex.lab=1.5)
lines(dta_tree, col = "darkred",lwd=2)
lines(dta_shrub, col = "darkblue",lwd=2)
lines(dta_grass, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
mtext("Leafout",side=2,line=6, cex=1.2)

#legend("topright", c("Trees","Shrubs","Forbs","Grass"), lty = c(1,1), col = c("darkgreen","darkblue","darkred",'goldenrod'), lwd=2,bty="n")

dta_treem <- density(treem, na.rm = TRUE)
dta_shrubm <- density(shrubm, na.rm = TRUE)
dta_forbm <- density(forbm, na.rm = TRUE)
dta_grassm <- density(grassm, na.rm = TRUE)

plot(dta_forbm, col ="darkgreen", main = " ", 
     xlim=c(-10,10),
     ylim = c(0, max(dta_forbm$y,dta_treem$y,dta_shrubm$y,dta_grassm$y)),
     lwd=2,bty="l", xlab="Effect on leafout per SD",
     cex.axis=1.5, cex.lab=1.5)
lines(dta_treem, col = "darkred",lwd=2)
lines(dta_shrubm, col = "darkblue",lwd=2)
lines(dta_grassm, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

dta_treeint <- density(treeint, na.rm = TRUE)
dta_shrubint <- density(shrubint, na.rm = TRUE)
dta_forbint <- density(forbint, na.rm = TRUE)
dta_grassint<- density(grassint, na.rm = TRUE)

plot(dta_forbint, col ="darkgreen", main = " ", 
     xlim=c(-10,10),
     ylim = c(0, max(dta_forbint$y,dta_treeint$y,dta_shrubint$y,dta_grassint$y)),
     lwd=2,bty="l", xlab="Effect on leafout per SD",
     cex.axis=1.5, cex.lab=1.5)
lines(dta_treeint, col = "darkred",lwd=2)
lines(dta_shrubint, col = "darkblue",lwd=2)
lines(dta_grassint, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
#dev.off()

#Flowering

flmod<- load("Analyses/output/brms/testm5cent.brms.ff.Rda")
mod<- testm5cent.ffd.brms
fit <- fixef(mod)
listofdraws <-as.data.frame(as.matrix(mod))
#no flowering stages for trees 
#mois
matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="shrub"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubm<-as.matrix(listofdraws%>% select(all_of(cols2use)))
#check: 
#colnames(shrubm)

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
#mean(shrubt)
#quantile(shrubt,probs=c(0.1,.9))

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

dta_shrub <- density(shrubt, na.rm = TRUE)
dta_forb <- density(forbt, na.rm = TRUE)
#dta_grass <- density(grasst, na.rm = TRUE)#not sure why grasses are not in model dataset

plot(dta_forb, col ="darkgreen", main = " ", 
     xlim=c(-40,40),
     ylim = c(0, max(dta_forb$y,dta_tree$y,dta_shrub$y,dta_grass$y)),
     lwd=2,bty="l", xlab="Effect on flowering per SD",
     cex.axis=1.5, cex.lab=1.5)
lines(dta_shrub, col = "darkblue",lwd=2)
#lines(dta_grass, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
mtext("Flowering",side=2,line=6, cex=1.2)

#legend("topright", c("Trees","Shrubs","Forbs","Grass"), lty = c(1,1), col = c("darkgreen","darkblue","darkred",'goldenrod'), lwd=2,bty="n")

dta_shrubm <- density(shrubm, na.rm = TRUE)
dta_forbm <- density(forbm, na.rm = TRUE)
#dta_grassm <- density(grassm, na.rm = TRUE)

plot(dta_forbm, col ="darkgreen", main = " ", 
     xlim=c(-10,10),
     ylim = c(0, max(dta_forbm$y,dta_treem$y,dta_shrubm$y,dta_grassm$y)),
     lwd=2,bty="l", xlab="Effect on flowerin per SD",
     cex.axis=1.5, cex.lab=1.5)
lines(dta_shrubm, col = "darkblue",lwd=2)
#lines(dta_grassm, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

dta_shrubint <- density(shrubint, na.rm = TRUE)
dta_forbint <- density(forbint, na.rm = TRUE)
#dta_grassint<- density(grassint, na.rm = TRUE)

plot(dta_forbint, col ="darkgreen", main = " ", 
     xlim=c(-10,10),
     ylim = c(0, max(dta_forbint$y,dta_treeint$y,dta_shrubint$y,dta_grassint$y)),
     lwd=2,bty="l", xlab="Effect on flowering per SD",
     cex.axis=1.5, cex.lab=1.5)
lines(dta_shrubint, col = "darkblue",lwd=2)
#lines(dta_grassint, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

dev.off()


#####################################
####### Ecosystem differences #######
#####################################
mod<- testm5cent.lod.brms
fit <- fixef(mod)

listofdraws <-as.data.frame(as.matrix(mod))

#moisture
matchExpression <- paste("r_sp[",loecos$spnum[loecos$ecosystem=="forest"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forestm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loecos$spnum[loecos$ecosystem=="grassland"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grasslandm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#check: 
#colnames(forestm)

#temperature
matchExpression <- paste("r_sp[",loecos$spnum[loecos$ecosystem=="forest"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forestt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loecos$spnum[loecos$ecosystem=="grassland"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grasslandt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#temp*mois interaction
matchExpression <- paste("r_sp[",loecos$spnum[loecos$ecosystem=="forest"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forestint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loecos$spnum[loecos$ecosystem=="grassland"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grasslandint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

figname<-"curvesloflecos.pdf"

#figname<-"curvesloecos.pdf"

pdf(file.path(figpath,figname), height=8,width=18)
#windows()
par(mfrow=c(2,3))

  dta_forest <- density(forestt, na.rm = TRUE)
  dta_grassland <- density(grasslandt, na.rm = TRUE)
  
  plot(dta_forest, col ="forestgreen", main = "Temperature", 
       xlim=c(-40,40),
       ylim = c(0, max(dta_forest$y,dta_grassland$y)),
       lwd=3,bty="l", xlab="Effect on leafout per SD",
       cex.axis=1.5, cex.lab=1.5)
  lines(dta_grassland, col = "goldenrod",lwd=3)
  #abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
  abline(v=0, lwd=1,col="black",lty=2)
  mtext("Leafout",side=2,line=6, cex=1.2)
  
  legend("topright", c("Forest","Grassland"), lty = c(1,1), col = c("forestgreen",'goldenrod'), lwd=2,bty="n")
  
  dta_forest <- density(forestt, na.rm = TRUE)
  dta_grassland <- density(grasslandt, na.rm = TRUE)
  
  #moisture
  dta_forestm <- density(forestm, na.rm = TRUE)
  dta_grasslandm <- density(grasslandm, na.rm = TRUE)
  
  plot(dta_forestm, col ="forestgreen", main = "Moisture", 
       xlim=c(-40,40),
       ylim = c(0, max(dta_forestm$y,dta_grasslandm$y)),
       lwd=3,bty="l", xlab="Effect on leafout per SD",
       cex.axis=1.5, cex.lab=1.5)
  lines(dta_grasslandm, col = "goldenrod",lwd=3)
  #abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
  abline(v=0, lwd=1,col="black",lty=2)
  mtext("Leafout",side=2,line=6, cex=1.2)
  
  #interaction
  dta_forestint <- density(forestint, na.rm = TRUE)
  dta_grasslandint <- density(grasslandint, na.rm = TRUE)
  
  plot(dta_forestint, col ="forestgreen", main = "Temperature*Moisture", 
       xlim=c(-40,40),
       ylim = c(0, max(dta_forestint$y,dta_grasslandint$y)),
       lwd=3,bty="l", xlab="Effect on leafout per SD",
       cex.axis=1.5, cex.lab=1.5)
  lines(dta_grasslandint, col = "goldenrod",lwd=3)
  #abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
  abline(v=0, lwd=1,col="black",lty=2)
  mtext("Leafout",side=2,line=6, cex=1.2)
  
#dev.off()  
 
#add flowering
#Flowering
  
  flmod<- load("Analyses/output/brms/testm5cent.brms.ff.Rda")
  mod<- testm5cent.ffd.brms
  fit <- fixef(mod)
  listofdraws <-as.data.frame(as.matrix(mod))
  
  
  #moisture
  matchExpression <- paste("r_sp[",ffecos$spnum[ffecos$ecosystem=="forest"],",mois]", collapse = NULL,sep="")
  cols2use<-intersect(colnames(listofdraws),matchExpression)
  forestm<-as.matrix(listofdraws%>% select(all_of(cols2use)))
  
  matchExpression <- paste("r_sp[",ffecos$spnum[ffecos$ecosystem=="grassland"],",mois]", collapse = NULL,sep="")
  cols2use<-intersect(colnames(listofdraws),matchExpression)
  grasslandm<-as.matrix(listofdraws%>% select(all_of(cols2use)))
  
  #check: 
  #colnames(forestm)
  
  #temperature
  matchExpression <- paste("r_sp[",ffecos$spnum[ffecos$ecosystem=="forest"],",temp]", collapse = NULL,sep="")
  cols2use<-intersect(colnames(listofdraws),matchExpression)
  forestt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

  matchExpression <- paste("r_sp[",ffecos$spnum[ffecos$ecosystem=="grassland"],",temp]", collapse = NULL,sep="")
  cols2use<-intersect(colnames(listofdraws),matchExpression)
  grasslandt<-as.matrix(listofdraws%>% select(all_of(cols2use)))
  
  #temp*mois interaction
  matchExpression <- paste("r_sp[",ffecos$spnum[ffecos$ecosystem=="forest"],",temp:mois]", collapse = NULL,sep="")
  cols2use<-intersect(colnames(listofdraws),matchExpression)
  forestint<-as.matrix(listofdraws%>% select(all_of(cols2use)))
  
  matchExpression <- paste("r_sp[",ffecos$spnum[ffecos$ecosystem=="grassland"],",temp:mois]", collapse = NULL,sep="")
  cols2use<-intersect(colnames(listofdraws),matchExpression)
  grasslandint<-as.matrix(listofdraws%>% select(all_of(cols2use)))
  
  dta_forest <- density(forestt, na.rm = TRUE)
  dta_grassland <- density(grasslandt, na.rm = TRUE)
  
  plot(dta_forest, col ="forestgreen", main = "Temperature", 
       xlim=c(-40,40),
       ylim = c(0, max(dta_forest$y,dta_grassland$y)),
       lwd=3,bty="l", xlab="Effect on flowering per SD",
       cex.axis=1.5, cex.lab=1.5)
  lines(dta_grassland, col = "goldenrod",lwd=3)
  #abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
  abline(v=0, lwd=1,col="black",lty=2)
  mtext("Flowering",side=2,line=6, cex=1.2)
  
  #legend("topright", c("Forest","Grassland"), lty = c(1,1), col = c("forestgreen",'goldenrod'), lwd=2,bty="n")
  
  dta_forest <- density(forestt, na.rm = TRUE)
  dta_grassland <- density(grasslandt, na.rm = TRUE)
  
  #moisture
  dta_forestm <- density(forestm, na.rm = TRUE)
  dta_grasslandm <- density(grasslandm, na.rm = TRUE)
  
  plot(dta_forestm, col ="forestgreen", main = "Moisture", 
       xlim=c(-40,40),
       ylim = c(0, max(dta_forestm$y,dta_grasslandm$y)),
       lwd=3,bty="l", xlab="Effect on flowering per SD",
       cex.axis=1.5, cex.lab=1.5)
  lines(dta_grasslandm, col = "goldenrod",lwd=3)
  #abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
  abline(v=0, lwd=1,col="black",lty=2)
  mtext("Flowering",side=2,line=6, cex=1.2)
  
  #interaction
  dta_forestint <- density(forestint, na.rm = TRUE)
  dta_grasslandint <- density(grasslandint, na.rm = TRUE)
  
  plot(dta_forestint, col ="forestgreen", main = "Temperature*Moisture", 
       xlim=c(-40,40),
       ylim = c(0, max(dta_forestint$y,dta_grasslandint$y)),
       lwd=3,bty="l", xlab="Effect on flowering per SD",
       cex.axis=1.5, cex.lab=1.5)
  lines(dta_grasslandint, col = "goldenrod",lwd=3)
  #abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
  abline(v=0, lwd=1,col="black",lty=2)
  mtext("Flowering",side=2,line=6, cex=1.2)
  
  dev.off()
  
###OLD CODE_ CAN PROBABLY DELETE?
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
