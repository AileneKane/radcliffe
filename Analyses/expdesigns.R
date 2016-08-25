### Started 28 June 2016 ###
### By Lizzie (so far) ###

## Looking at simple ANOVA results of warming experiments ##
## Given different estimates of warming and given exact design (e.g., blocks) ##

###############
## To do ... ##
###############
# How to deal wth 1.2,1.9 in the reported temperatures .... and the NA
# Think about default contrasts (options(contrasts=c("contr.sum", "contr.poly"))
# Species .... just treating it as ranef at intercept, definitely not ideal but easy
# 104 BACE observations with no block

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
## First, let's look at Cleland, the only split-plot (split-plot, the plot is quadrant-plot in this case)

# Based on Zavaleta et al. 2003 (PNAS):
# Warming applied at plot level (as was CO2)
# Precip (and N dep) were applied within plots at the quadrant level

jasper <- subset(phendat, site=="cleland")

jr.split <- lmer(doy~temptreat*preciptreat + (1|temptreat:block) + (1|year) +
    (1|latbi), data=jasper)
jr.block <- lmer(doy~temptreat*preciptreat + (1|block) + (1|year) + (1|latbi), data=jasper)

anova(jr.block, jr.split)

# but does appropriately handle the repeated measures aspect? Below are not very happy
jr.split.rm <- lmer(doy~temptreat*preciptreat*year + (1|temptreat:block) + (1|block/plot) +
    (1|latbi), data=jasper) # cannot nest plot in block here: (1|temptreat:block/plot)
jr.block.rm <- lmer(doy~temptreat*preciptreat*year + (1|block/plot) +
    (1|latbi), data=jasper)

anova(jr.split.rm, jr.block.rm)

##
## Does including block impact findings?
phenblock <- phendat[which(!phendat$site %in% noblocks),]
phenblock.noprecip <- subset(phenblock, preciptreat==0 | is.na(preciptreat)==TRUE)
phenblock.wprecip <- phenblock[which(phenblock$site %in% ayprecip),]

# start with temp only, year as fixed
ignoreblock <- lmer(doy~temptreat*year + (1|site/plot) + (1|latbi), data=phenblock.noprecip,
    na.action=na.exclude)
block <- lmer(doy~temptreat*year + (1|site/block/plot) + (1|latbi), data=phenblock.noprecip,
    na.action=na.exclude)
block.ranefyr <- lmer(doy~temptreat + (1|site/block/plot) + (1|latbi) + (1|year), data=phenblock.noprecip,
    na.action=na.exclude)

summary(ignoreblock) # plot:site = 3.7
summary(block) # variance explained by block and plot = 3.76
summary(block.ranefyr) # hmm

anova(ignoreblock)
anova(block)
anova(block.ranefyr)

# Take homes: blocking doesn't do much for you. Year is a huge effect.

# same for precip*temp studies
ignoreblock.p <- lmer(doy~temptreat*preciptreat*year + (1|site/plot) + (1|latbi), data=phenblock.wprecip,
    na.action=na.exclude)
block.p <- lmer(doy~temptreat*preciptreat*year + (1|site/block/plot) + (1|latbi), data=phenblock.wprecip,
    na.action=na.exclude)
block.ranefyr.p <- lmer(doy~temptreat*preciptreat + (1|site/block/plot) + (1|latbi) + (1|year), data=phenblock.wprecip,
    na.action=na.exclude)

# note the warnings!
summary(ignoreblock.p) # plot:site = 3.8
summary(block.p) # 3.8 again, really nothing added by block I don't think?
summary(block.ranefyr.p) # hmm

anova(ignoreblock.p)
anova(block.p)
anova(block.ranefyr.p)

stop(print("stop, the below code is in progress!"))

##
## START HERE!
##

mode(phendat$target)
mode(phendat$reported)

# effects of target versus reported temp? Not much.
temp.target <- lmer(doy~target + (1|site/block/plot) + (1|latbi) + (1|year), data=phendat.noprecip.wrep,
    na.action=na.exclude)
temp.reported <- lmer(doy~reported + (1|site/block/plot) + (1|latbi) + (1|year), data=phendat.noprecip.wrep,
    na.action=na.exclude)

summary(temp.target)
summary(temp.reported) # super similar estimates of -3.6 days (I think this is almost exactly what I got in Fig S8 in my paper for above-canopy heaters!) 


## Real temperatures ...
phendat.air <- subset(phendat.noprecip, is.na(AGtemp_mean_dev)==FALSE)
phendat.soil <- subset(phendat.noprecip, is.na(BGtemp_mean_dev)==FALSE)

## soil temps from 9 studies
ggplot(phendat.soil, aes(x=year, y=doy, color=genus)) + 
    facet_wrap(~site, scales="free_x") +
    geom_point()
## air temps from 6 studies
ggplot(phendat.air, aes(x=year, y=doy, color=genus)) + 
    facet_wrap(~site, scales="free_x") +
    geom_point()


temp.rep.air <- lmer(doy~target + (1|site/plot) + (1|latbi) + (1|year), 
    data=phendat.air, na.action=na.exclude) # block not sampled enough in this data subset
temp.rep.soil <- lmer(doy~target + (1|site/block/plot) + (1|latbi) + (1|year),
    data=phendat.soil, na.action=na.exclude) 

temp.real.air <- lmer(doy~AGtemp_mean_dev + (1|site/plot) + (1|latbi) + (1|year), 
    data=phendat.air, na.action=na.exclude) # block not sampled enough in this data subset
temp.real.soil <- lmer(doy~BGtemp_mean_dev + (1|site/block/plot) + (1|latbi) + (1|year),
    data=phendat.soil, na.action=na.exclude) 

summary(temp.rep.air) # 0.05, weird, need to look into
summary(temp.real.air) # -2.3

summary(temp.rep.soil) # -1.27
summary(temp.real.soil) # effect of real temp is almost double

# does max air or min matter more?
temp.real.air.min <- lmer(doy~AGtemp_min_dev + (1|site/plot) + (1|latbi) + (1|year), 
    data=phendat.air, na.action=na.exclude)
temp.real.air.max <- lmer(doy~AGtemp_max_dev + (1|site/plot) + (1|latbi) + (1|year), 
    data=phendat.air, na.action=na.exclude)

summary(temp.real.air) # -2.33
summary(temp.real.air.min) # -1.9
summary(temp.real.air.max) # -1
# Hmm, the mean is so much more meaningful perhaps?



