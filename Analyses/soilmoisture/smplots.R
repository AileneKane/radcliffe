# Plotting BB vs soil moisture, by site
#Does there appear to be a threshold in soil moisture effect on budburst

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE
library(lme4)
library(rstan)
library(ggplot2)
library(shinystan)
library(bayesplot)
library(rstanarm)
library(dplyr)
library(brms)
library(RColorBrewer)
library(plotrix)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#options(mc.cores = parallel::detectCores()) # added by Andrew
#update.packages()

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/radcliffe")}
#setwd("~/Documents/GitHub/radcliffe")#noaa
#Goal: Fit a multi-model to phenology (budburst) data with temperature, soil moisture, and 
#their interaction as explanatory variables.
#
###Now with the data

#source('Analyses/soilmoisture/savestan.R')

#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)

#Make some choices about how to restrict the data:
remove.conifers=TRUE
use.airtemp=TRUE
#standard data wrangling to get expclim2 for climate analyses and expgdd for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")

#Summarize climate data by plot (annual and seasonal temp, soil mois), 
#merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
#source("Analyses/soilmoisture/climsum_byplot_soiltoo.R")#doesn't work for some reason....
source("Analyses/source/climsum_byplot.R")

#Prepare data for phenology models in stan#####
source("Analyses/source/stanprep_phenmods.R")

######start with budburst
sites<-unique(expgdd_bbd$site2)
pdf(file ="Analyses/figures/sitesm.bbd.pdf", width = 8, height = 6)
  par(mfrow= c(2,3))
    for(i in 1:length(sites)){
  sitedat<-expgdd_bbd[expgdd_bbd$site2 == sites[i],]
      plot(sitedat$soilmois_janmar,sitedat$doy, main = paste(sites[i]," bbd", sep = ""), 
           ylim= c(min(expgdd_bbd$doy),max(expgdd_bbd$doy)),
           xlim=c(min(expgdd_bbd$soilmois_janmar), max(expgdd_bbd$soilmois_janmar)))
}
dev.off()

sites<-unique(expgdd_lod$site2)
pdf(file ="Analyses/figures/sitesm.lod.pdf", width = 8, height = 6)
par(mfrow= c(2,3))
for(i in 1:length(sites)){
  sitedat<-expgdd_lod[expgdd_lod$site2 == sites[i],]
  plot(sitedat$soilmois_janmar,sitedat$doy, main = paste(sites[i]," lod", sep = ""), 
       ylim= c(min(expgdd_lod$doy),max(expgdd_lod$doy)),
       xlim=c(min(expgdd_lod$soilmois_janmar), max(expgdd_lod$soilmois_janmar)))
}
dev.off()

sites<-unique(expgdd_ffd$site2)
pdf(file ="Analyses/figures/sitesm.ffd.pdf", width = 8, height = 6)
par(mfrow= c(2,3))
for(i in 1:length(sites)){
  sitedat<-expgdd_ffd[expgdd_ffd$site2 == sites[i],]
  plot(sitedat$soilmois_janmar,sitedat$doy, main = paste(sites[i]," ffd", sep = ""), 
       ylim= c(min(expgdd_ffd$doy),max(expgdd_ffd$doy)),
       xlim=c(min(expgdd_ffd$soilmois_janmar), max(expgdd_ffd$soilmois_janmar)))
}
dev.off()