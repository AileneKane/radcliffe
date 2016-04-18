### Started 18 April 2016 ###
### By Lizzie ###

## Code taken from sppoverlap.R ##
## This code looks at obs data from UWM, Harvard, RMBL and Konza #
## and figures out which species overlap from the experimental sites ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## setup
library(ggplot2)
setwd("~/Documents/git/projects/meta_ep2/radcliffe/Analyses")

# get the data
obs <- read.csv("obspheno.csv", header=TRUE)
exp <- read.csv("exppheno.csv", header=TRUE)
treats <- read.csv("treats.csv", header=TRUE)
treats$X <- NULL

exp$latbi <- paste(exp$genus, exp$species)
obs$latbi <- paste(obs$genus, obs$species)

obs4sites <- subset(obs, site=="harvard"|site=="uwm"|
    site=="gothic"|site=="konza")
unique(obs4sites$site) # check

obspp <- unique(obs4sites$latbi)
expspp <- unique(exp$latbi)

length(obspp)
length(expspp)

# how many overlapping spp?
sort(expspp[which(expspp %in% obspp)]) # 65

# in which experiments
expobsspp <- exp[which(exp$latbi %in% obspp),]
unique(expobsspp$site)

for (i in 1:length(unique(expobsspp$site))){
    subby <- subset(expobsspp, site==unique(expobsspp$site)[i])
    print(paste("for ", unique(expobsspp$site)[i], ":", sep=""))
    print(unique(subby$latbi))
}

