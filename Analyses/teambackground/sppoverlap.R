### Started 4 April 2016 ###
### By Lizzie ### 

## Quick look at overlapping species between experiments and observations ##
## And those we can get range maps for ##

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

obspp <- unique(obs$latbi)
expspp <- unique(exp$latbi)

length(obspp)
length(expspp)

# how many overlapping spp?
sort(expspp[which(expspp %in% obspp)]) # yay 145

# subset the observational data down to overlappings spp
obsexpspp <- obs[which(obs$latbi %in% expspp),]
expobsspp <- exp[which(exp$latbi %in% obspp),]

length(unique(expobsspp$site)) # all 12 sites
length(unique(obsexpspp$site)) # only 11 of the 15 sites

# table(expobsspp$site, expobsspp$latbi)
# which experiments have usable air temp: 7 sites have some form of usable air temp now
# Jasper is question mark, and so is Farnsworth
siteswnoair <- c("dunne", "price") 
length(unique(paste(expobsspp$site, expobsspp$latbi))) # species are often at more than one site 

table(obsexpspp$latbi)

obsnperspp <- aggregate(obsexpspp["year"], obsexpspp[c("latbi", "site", "event")], FUN=length)
obsnperspp.noevent <- aggregate(obsexpspp["year"], obsexpspp[c("latbi", "site")],
    FUN=length)
dim(obsnperspp.noevent)

## how many could we get ranges for?
wee <- read.delim("teambackground/input/littlemaps.txt", header=TRUE, sep="\t") # damn, 679 species ranges!

rangespp <- unique(obsexpspp$latbi)[which(unique(obsexpspp$latbi) %in% wee$Latin.Name)] # 48!!!

obsexpspp.wr <- obsexpspp[which(obsexpspp$latbi %in% rangespp),]
expobsspp.wr <- expobsspp[which(expobsspp$latbi %in% rangespp),]
# just above we lose sherry and chuine and cleland experimental site

## how many species do we have if we drop RMBL, which may not have air temp...
expobsspp.noair <- expobsspp.wr[which(!expobsspp.wr$site %in% siteswnoair),]
length(unique(expobsspp.noair$latbi)) # we lose one species (Artemisia tridentata)

## okay, we agreed to toss RMBL experiment (not so many species) and to exclude data before 1 Jan 1901
## we *also* decided the following rules to keep species in the list
## (1) 2 years for each species within OBS site
## (2) 2 OBS per exp site where EACH is in a different temp treatment 

## merge the data
obsexpspp.wr.sm <- subset(obsexpspp.wr, select=c("site", "plot", "latbi", "event", "doy", "year"))
expobsspp.wr.sm <- subset(expobsspp.wr, select=c("site", "plot", "latbi", "event", "doy", "year"))
obsexpspp.wr.sm <- obsexpspp.wr.sm[,c("site", "plot", "latbi", "event", "doy", "year")]
expobsspp.wr.sm <- expobsspp.wr.sm[,c("site", "plot", "latbi", "event", "doy", "year")]
expobsspp.wr.sm$type <- "exp"
obsexpspp.wr.sm$type <- "obs"

allexpobs <- rbind(expobsspp.wr.sm, obsexpspp.wr.sm)

## drop Dunne and Price (RMBL experiments) and Gothic
# since now we have no matching experiment
allexpobs.use <- allexpobs[which(!allexpobs$site %in% siteswnoair),]
allexpobs.use <- subset(allexpobs.use, site != "gothic")
allexpobs.use <- subset(allexpobs.use, year>1900)

## merge in treatment data and make some useful columns
allexpobs.use.wtreat <- merge(allexpobs.use, treats, by=c("site", "plot"), all.x=TRUE)
allexpobs.use.wtreat$fulltreat <- paste(allexpobs.use.wtreat$temptreat,
    allexpobs.use.wtreat$preciptreat)
allexpobs.use.wtreat$sp.event <- paste(allexpobs.use.wtreat$latbi,
    allexpobs.use.wtreat$event)

## start counts
allexpobs.use.wtreat.agg <- aggregate(allexpobs.use.wtreat[c("doy")],
   allexpobs.use.wtreat[c("site", "latbi", "fulltreat", "sp.event", "year", "type")], FUN=mean)
countz <- aggregate(allexpobs.use.wtreat.agg[c("doy")],
   allexpobs.use.wtreat.agg[c("site", "latbi","sp.event", "type")], FUN=length)
countz.use <- subset(countz, doy>2)

unique(countz.use$latbi)
unique(countz.use$site)

table(countz.use$latbi, countz.use$site)

getsites <- subset(countz.use, select=c("site", "type"))
getsites <- aggregate(getsites[c("type")], getsites[c("site", "type")], FUN=length)
speciesatthend <- sort(unique(countz.use$latbi))

# we lost cleland, sherry and chuine ... 
getsites

write.csv(speciesatthend, "teambackground/output/speciestogetCRU.csv", row.names=FALSE)
write.csv(getsites[,1:2], "teambackground/output/sitestogetCRU.csv", row.names=FALSE)


## look! Lizzie uses facet!
meplot <- ggplot(allexpobs.use, aes(x=event, y=doy, fill=latbi)) + 
  geom_boxplot()
meplot + facet_wrap(~site, scale="free_x")
