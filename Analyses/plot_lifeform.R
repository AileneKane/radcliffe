## Started April 2021 ##
## By Ailene ##

## plot species estimates by lifeform

############################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

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

figname<-"hist.loform.pdf"

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

matchExpression <- paste(loform$spnumlo[loform$form=="shrub"],",temp]", collapse = "|",sep="")
shrubt<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(loform$spnumlo[loform$form=="forb"],",temp]", collapse = "|",sep="")
forbt<-as.matrix(listofdraws%>% select(matches(matchExpression)))

matchExpression <- paste(loform$spnumlo[loform$form=="grass"],",temp]", collapse = "|",sep="")
grasst<-as.matrix(listofdraws%>% select(matches(matchExpression)))

pdf(file.path(figpath,figname), height=12,width=4)
#quartz(height=10,width=4)
par(mfcol=c(4,2))
hist(treet, main="Trees",xlim=c(-30,30),col="darkred",xlab="Temp effects")
hist(shrubt, main="Shrubs",xlim=c(-30,30),col="darkred",xlab="Temp effects")
hist(forbt, main="Herbs",xlim=c(-30,30),col="darkred",xlab="Temp effects")
hist(grasst, main="Grass",xlim=c(-30,30),col="darkred",xlab="Temp effects")
hist(treem, main="Trees",xlim=c(-30,30),col="darkblue",xlab="Mois effects")
hist(shrubm, main="Shrubs",xlim=c(-30,30),col="darkblue",xlab="Mois effects")
hist(forbm, main="Herbs",xlim=c(-30,30),col="darkblue",xlab="Mois effects")
hist(grassm, main="Grass",xlim=c(-30,30),col="darkblue",xlab="Mois effects")
dev.off()
