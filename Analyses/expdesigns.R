### Started 28 June 2016 ###
### By Lizzie (so far) ###

## Looking at simple ANOVA results of warming experiments ##
## Given different estimates of warming and given exact design (e.g., blocks) ##

###############
## To do ... ##
###############
# See if Ailene can build long format of expsite? 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(plyr)
library(nlme)

setwd("~/Documents/git/projects/meta_ep2/radcliffe/Analyses")

## get the data
# Christy says (early June 2016): I just pushed two sets of calculations: one at the plot level (EffectiveWarming_Plot.csv) and one at the treatment level (EffectiveWarming_Treatment.csv). Because not all treatments have above or below ground temperature, I went ahead and calculated the deviation from control/ambient for aboveground and belowground max, min, and mean temperature.
effwarm <- read.csv("EffectiveWarming_Treatment.csv", header=TRUE)
effwarm.plot <- read.csv("EffectiveWarming_Plot.csv", header=TRUE)
treats <- read.csv("Treats.csv", header=TRUE)

expphen <- read.csv("exppheno.csv", header=TRUE)
expsite <- read.csv("expsiteinfo.csv", header=TRUE)

# summaries
table(effwarm.plot$site, effwarm.plot$temptreat)
table(effwarm.plot$site, effwarm.plot$temptreat, effwarm.plot$preciptreat)

# these need to match ...
unique(paste(effwarm.plot$site, effwarm.plot$temptreat))
unique(paste(effwarm$site, effwarm$temptreat))
unique(paste(treats$site, treats$temptreat))

# just looking at treatements ...
effwarm.plot.2temp <- subset(effwarm.plot, temptreat<3)
table(effwarm.plot.2temp$site, effwarm.plot.2temp$temptreat)

# just looking around some more
chuine.clim <- subset(effwarm.plot, site=="chuine")
chuine.phen <- subset(expphen, site=="chuine")

unique(chuine.clim$plot)
unique(chuine.phen$plot) # these don't merge!

## try some merges
goo <- merge(expphen, treats, by=c("site", "plot")) # losing about 17% of the data
goo <- merge(goo, effwarm.plot, by=c("site", "plot")) # losing 80% of resulting data
