### Started 28 June 2016 ###
### By Lizzie (so far) ###

## Looking at simple ANOVA results of warming experiments ##
## Given different estimates of warming and given exact design (e.g., blocks) ##

################################
## Read me, notes from Lizzie ##
################################
# Merge in a file that has the different target/reported temperatures by Year
# Think about default contrasts (options(contrasts=c("contr.sum", "contr.poly"))
# Species .... just treating it as ranef at intercept, definitely not ideal but easy, really need to get into slopes I think. 
# 104 BACE observations with no block: Ailene says these are plots where phonology data were collected, but there is no climate data. they are separate from the experimental setup (maybe remove?)
# Watch out on including block as not all studies have it (you thus lose lots of data)
# Year ... think more, continuous variable? Fixef or Ranef?
# And don't forget! Events (BBD or FLD, for example). Need to consider. ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)

setwd("~/Documents/git/projects/meta_ep2/radcliffe/analyses")

## get the data
# Christy says (early June 2016): I just pushed two sets of calculations: one at the plot level (EffectiveWarming_Plot.csv) and one at the treatment level (EffectiveWarming_Treatment.csv). Because not all treatments have above or below ground temperature, I went ahead and calculated the deviation from control/ambient for aboveground and belowground max, min, and mean temperature.

effwarm <- read.csv("EffectiveWarming_Treatment.csv", header=TRUE)
effwarm.plot <- read.csv("EffectiveWarming_Plot.csv", header=TRUE)
treats <- read.csv("treats_detail.csv", header=TRUE)

expphen <- read.csv("exppheno.csv", header=TRUE)
expsite <- read.csv("expsiteinfo.csv", header=TRUE)

# summaries
table(effwarm.plot$site, effwarm.plot$temptreat)
table(effwarm.plot$site, effwarm.plot$preciptreat)
table(effwarm.plot$site, effwarm.plot$temptreat, effwarm.plot$preciptreat)

ayprecip <- c("bace", "chuine", "cleland", "force", "sherry")

# these need to match ...
sort(unique(paste(effwarm.plot$site, effwarm.plot$temptreat)))
sort(unique(paste(effwarm$site, effwarm$temptreat)))
sort(unique(paste(treats$site, treats$temptreat)))

# just looking at treatements ...
effwarm.plot.2temp <- subset(effwarm.plot, temptreat<3)
table(effwarm.plot.2temp$site, effwarm.plot.2temp$temptreat)

# just looking around some more
chuine.clim <- subset(effwarm.plot, site=="chuine")
chuine.phen <- subset(expphen, site=="chuine")

unique(chuine.clim$plot)
unique(chuine.phen$plot) # these don't merge!

## merge the data! Thank you Ailene
phendat.simple <- join(expphen, treats, by=c("site", "block", "plot")) 
phendat <- join(phendat.simple, effwarm.plot, by=c("site", "block", "plot","temptreat",
    "preciptreat"), match="first")

phendat$target <- as.numeric(phendat$target)
phendat$latbi <- paste(phendat$genus, phendat$species)
phendat$yr <- phendat$year-1980 # a little scaling

## ask a few questions
whoblocks <- ddply(phendat, c("site"), count,
      block=block)
noblocks <- c("clarkduke", "clarkharvard", "dunne", "ellison", "marchin", "price", "sherry")

specreps <- ddply(phendat, c("latbi"), count,
      site=site)


## need to clean up ...

phendat$reported <- as.numeric(phendat$reported)
phendat.noprecip <- subset(phendat, preciptreat==0 | is.na(preciptreat)==TRUE)
phendat.noprecip.wrep <- subset(phendat.noprecip, is.na(reported)==FALSE)

# Remember (maybe better to think of as a factor at some point?)
mode(phendat$year)

##
## plot

ggplot(phendat, aes(x=year, y=doy, color=genus)) + 
    facet_wrap(~site, scales="free_x") +
    geom_point()


##
## First, let's look at Cleland, the only split-plot (the plot is quadrant-within-plot in this case)

# Based on Zavaleta et al. 2003 (PNAS):
# Warming applied at plot level (as was CO2)
# Precip (and N dep) were applied within plots at the quadrant level

jasper <- subset(phendat, site=="cleland")

jr.split <- lmer(doy~temptreat*preciptreat + (1|temptreat:block) + (1|yr) +
    (1|latbi), data=jasper)
jr.block <- lmer(doy~temptreat*preciptreat + (1|block) + (1|yr) + (1|latbi), data=jasper)

anova(jr.block, jr.split)

# but does appropriately handle the repeated measures aspect? Below are not very happy
jr.split.rm <- lmer(doy~temptreat*preciptreat*yr + (1|temptreat:block) + (1|block/plot) +
    (1|latbi), data=jasper) # cannot nest plot in block here: (1|temptreat:block/plot)
jr.block.rm <- lmer(doy~temptreat*preciptreat*yr + (1|block/plot) +
    (1|latbi), data=jasper)

anova(jr.split.rm, jr.block.rm)

##
## Does including block impact findings?
phenblock <- phendat[which(!phendat$site %in% noblocks),]
phenblock.noprecip <- subset(phenblock, preciptreat==0 | is.na(preciptreat)==TRUE)
phenblock.wprecip <- phenblock[which(phenblock$site %in% ayprecip),]

# start with temp only, yr as fixed
ignoreblock <- lmer(doy~temptreat*yr + (1|site/plot) + (1|latbi), data=phenblock.noprecip,
    na.action=na.exclude)
block <- lmer(doy~temptreat*yr + (1|site/block/plot) + (1|latbi), data=phenblock.noprecip,
    na.action=na.exclude)
block.ranefyr <- lmer(doy~temptreat + (1|site/block/plot) + (1|latbi) + (1|yr), data=phenblock.noprecip,
    na.action=na.exclude)

summary(ignoreblock) # plot:site = 3.7
summary(block) # variance explained by block and plot = 3.76
summary(block.ranefyr) # hmm .. (note: 7K obs)

anova(ignoreblock)
anova(block)
anova(block.ranefyr)

# Take homes: blocking doesn't do much for you. Yr is a huge effect.

# same for precip*temp studies
ignoreblock.p <- lmer(doy~temptreat*preciptreat*yr + (1|site/plot) + (1|latbi), data=phenblock.wprecip,
    na.action=na.exclude)
block.p <- lmer(doy~temptreat*preciptreat*yr + (1|site/block/plot) + (1|latbi), data=phenblock.wprecip,
    na.action=na.exclude)
block.ranefyr.p <- lmer(doy~temptreat*preciptreat + (1|site/block/plot) + (1|latbi) + (1|yr), data=phenblock.wprecip,
    na.action=na.exclude)

# note the warnings!
summary(ignoreblock.p) # plot:site = 3.8
summary(block.p) # 3.8 again, really not much added by block I don't think?
summary(block.ranefyr.p) # hmm

anova(ignoreblock.p)
anova(block.p)
anova(block.ranefyr.p)


# stop(print("stop, the below code is in progress!"))

mode(phendat$target)
mode(phendat$reported)

# effects of target versus reported temp? Not much.
temp.target <- lmer(doy~target + (1|site/block/plot) + (1|latbi) + (1|yr), data=phendat.noprecip.wrep,
    na.action=na.exclude)
temp.reported <- lmer(doy~reported + (1|site/block/plot) + (1|latbi) + (1|yr), data=phendat.noprecip.wrep,
    na.action=na.exclude)

summary(temp.target)
summary(temp.reported)
# super similar estimates of -3.6 days (in my 2012 paper I got -3 (2.9 something), see in Fig S8 in my paper for above-canopy heaters) ... but based on 3K obs because of block ...

# effects of target versus reported temp? Not much.
temp.target.noblock <- lmer(doy~target + (1|site/plot) + (1|latbi) + (1|yr), data=phendat.noprecip.wrep,
    na.action=na.exclude)
temp.reported.noblock <- lmer(doy~reported + (1|site/plot) + (1|latbi) + (1|yr), data=phendat.noprecip.wrep,
    na.action=na.exclude)

summary(temp.target.noblock) # -0.37
summary(temp.reported.noblock) # -0.57

unique(phendat.noprecip.wrep$site)

## Real temperatures ...
phendat.air <- subset(phendat.noprecip, is.na(AGtemp_mean_dev)==FALSE)
phendat.soil <- subset(phendat.noprecip, is.na(BGtemp_mean_dev)==FALSE)

unique(phendat.air$site)
unique(phendat.soil$site)

## soil temps from 9 studies
ggplot(phendat.soil, aes(x=yr, y=doy, color=genus)) + 
    facet_wrap(~site, scales="free_x") +
    geom_point()
## air temps from 6 studies
ggplot(phendat.air, aes(x=yr, y=doy, color=genus)) + 
    facet_wrap(~site, scales="free_x") +
    geom_point()


temp.tar.air <- lmer(doy~target + (1|site/plot) + (1|latbi) + (1|yr), 
    data=phendat.air, na.action=na.exclude) # block not sampled enough in this data subset
temp.tar.soil <- lmer(doy~target + (1|site/plot) + (1|latbi) + (1|yr),
    data=phendat.soil, na.action=na.exclude) 

temp.rep.air <- lmer(doy~reported + (1|site/plot) + (1|latbi) + (1|yr), 
    data=phendat.air, na.action=na.exclude) # block not sampled enough in this data subset
temp.rep.soil <- lmer(doy~reported + (1|site/plot) + (1|latbi) + (1|yr),
    data=phendat.soil, na.action=na.exclude) # block not sampled enough in this data subset

temp.real.air <- lmer(doy~AGtemp_mean_dev + (1|site/plot) + (1|latbi) + (1|yr), 
    data=phendat.air, na.action=na.exclude) # block not sampled enough in this data subset
temp.real.soil <- lmer(doy~BGtemp_mean_dev + (1|site/plot) + (1|latbi) + (1|yr),
    data=phendat.soil, na.action=na.exclude)


summary(temp.tar.air) # 0.05 (22K obs)
summary(temp.rep.air) # 0.71 (22K obs)
summary(temp.real.air) # -2.3 (48K obs)

summary(temp.tar.soil) # 0.7 (26K obs)
summary(temp.rep.soil) # 0.09 (23K obs)
summary(temp.real.soil) # -2.514 (55K obs)

# does max air or min matter more?
temp.real.air.min <- lmer(doy~AGtemp_min_dev + (1|site/plot) + (1|latbi) + (1|yr), 
    data=phendat.air, na.action=na.exclude)
temp.real.air.max <- lmer(doy~AGtemp_max_dev + (1|site/plot) + (1|latbi) + (1|yr), 
    data=phendat.air, na.action=na.exclude)

summary(temp.real.air) # -2.33
summary(temp.real.air.min) # -1.9
summary(temp.real.air.max) # -1
# Hmm, the mean is so much more meaningful perhaps?



