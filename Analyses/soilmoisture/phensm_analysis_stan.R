#Started Sept 2017
#By Ailene

#Use Stan to fit soil moisture- phenology model to radcliffe data 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

library(rstan)
library(ggplot2)
library(shinystan)
library(bayesplot)
library(rstanarm)
#update.packages()
# Setting working directory. Add in your own path in an if statement for your file structure
 if(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/radcliffe")}

source('Analyses/soilmoisture/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)

#standard data wrangling to get expclim2 for climate analyses and expgdd for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")

#summarize climate data by plot (annual and seasonal temp, soil mois), 
  #merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
source("Analyses/soilmoisture/climsum_byplot.R")

#make things numbers that need to be for stan
#source("Analyses/soilmoisture/stanprep.R")

#Prep the data for Stan model

#1) Divide by phenophase:

expgdd_bbd<-expgdd_subs[which(expgdd_subs$event=="bbd"),]#bud burst data
expgdd_bbd <- expgdd_bbd[apply(expgdd_bbd, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
expgdd_bbd$genus.species<-as.numeric(as.factor(expgdd_bbd$genus.species))

expgdd_lod<-expgdd_subs[which(expgdd_subs$event=="lod"),]#leaf out data

expgdd_lud<-expgdd_subs[which(expgdd_subs$event=="lud"),]#leaf unfolding data

## For centering data, not doing it for now:
#expgdd_bbd$sm_cent <- scale(expgdd_bbd$sm, center=TRUE, scale=TRUE)
#expgdd_bbd$smjm_cent<-scale(expgdd_bbd$soilmois_janmar, center = TRUE, scale = TRUE)
#expgdd_bbd$ag_min_jm_cent<-scale(expgdd_bbd$ag_min_janmar, center = TRUE, scale = TRUE)
#expgdd_bbd$agtmax_cent<-scale(expgdd_bbd$agtmax, center = TRUE, scale = TRUE)


#2) Make a list out of the processed data. It will be input for the model.

#Expecting a single string value: [type=character; extent=6].
datalist.bbd <- with(expgdd_bbd, 
                    list(y = doy, 
                         temp = ag_min_janmar, #above-ground minimum air temp
                         mois = soilmois_janmar, #soil moisture
                         sp = genus.species,
                         N = nrow(expgdd_bbd),
                         n_sp = length(unique(expgdd_bbd$genus.species))
                    )
)

######################################
# m1: a(sp) + t(sp) + m(sp) + t*m(sp) 
#######################################################
m1 = stan('Analyses/soilmoisture/M1_bbd.stan', data = datalist.bbd,
               iter = 2500, warmup=1500) # 
 
gsave(m1, file="M1_bbd.Rda")

m1.sum <- summary(ml)$summary 
head(m1.sum) 
m1.sum[grep("mu_a_sp", rownames(m1.sum)),]
m1.sum[grep("b_t", rownames(m1.sum)),]
m1.sum[grep("b_cp", rownames(m1.sum)),]
m1.sum[grep("b_fp", rownames(m1.sum)),]
# 