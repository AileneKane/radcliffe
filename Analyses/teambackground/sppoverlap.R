### Started 4 April 2016 ###
### By Lizzie ### 

## Quick look at overlapping species between experiments and observations ##
## And those we can get range maps for ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/git/projects/meta_ep2/radcliffe/Analyses")

# get the data
obs <- read.csv("obspheno.csv", header=TRUE)
exp <- read.csv("exppheno.csv", header=TRUE)

exp$latbi <- paste(exp$genus, exp$species)
obs$latbi <- paste(obs$genus, obs$species)

obspp <- unique(obs$latbi)
expspp <- unique(exp$latbi)

length(obspp)
length(expspp)

# how many overlapping spp?
sort(expspp[which(expspp %in% obspp)]) # yay 145

# subset the observational data down to overlappings spp
obsexpspp <- obs[which(obs$latbi %in% expspp),]
table(obsexpspp$latbi)

obsnperspp <- aggregate(obsexpspp["year"], obsexpspp[c("latbi", "site", "event")], FUN=length)
obsnperspp.noevent <- aggregate(obsexpspp["year"], obsexpspp[c("latbi", "site")],
    FUN=length)
dim(obsnperspp.noevent)

## how many could we get ranges for?
wee <- read.delim("teambackground/input/littlemaps.txt", header=TRUE, sep="\t") # damn, 679 species ranges!

unique(obsexpspp$latbi)[which(unique(obsexpspp$latbi) %in% wee$Latin.Name)] # 48!!!
