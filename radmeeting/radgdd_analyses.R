### Started 3 April 2016 ###
### By Lizzie (for now ###

### Looking at the GDD crit ### 

## choose 2 or 4 base, do air and soil separate
## look at phen by event type

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## setup
library(ggplot2)
library(tidyr) # I don't actually use this below, but I *want* to
library(plyr)
library(lme4)

## set working directory (to each his own)
# setwd("~/GitHub/radcliffe/radmeeting")
setwd("~/Documents/git/projects/meta_ep2/radcliffe")

gdd <- read.csv("radmeeting/gddest.csv", header=TRUE)

## let's start small and look at one site
## bace, sherry and force have multiple levels of precip
## only bace has multiple temp and precip treatments

##
## bace
##

bace <- subset(gdd, site=="bace")
plot(gdd.est~tbase, data=bace) # perhaps not suprisingly there is a fundamental relationship between GDD and tbase (lower tbase, higher GDD)

# we'll look quickly at base 2 and base 10 to see if they change what you see
unique(bace$species)
ggplot(subset(bace, tbase==2), aes(x=temptreat, y=gdd.est, fill=species)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)
quartz()
ggplot(subset(bace, species=="Acer.rubrum" & tbase==2),
  aes(x=temptreat, y=gdd.est, fill=as.factor(preciptreat))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)

ggplot(subset(bace, tbase==10), aes(x=temptreat, y=gdd.est, fill=species)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)
quartz()
ggplot(subset(bace, species=="Acer.rubrum" & tbase==10),
  aes(x=temptreat, y=gdd.est, fill=as.factor(preciptreat))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)

# under base 10 you get more variability in some treatments and the droughts are more similar for temp=0|1 than in base 2, but then less similar in  temp=2|3 ... also, note how very small the GDDs are


##
## sherry
##

sher <- subset(gdd, site=="sherry")
plot(gdd.est~tbase, data=sher) # ditto what I said above

# we'll look quickly at base 2 and base 10 to see if they change what you see
unique(sher$species)
ggplot(subset(sher, tbase==2), aes(x=temptreat, y=gdd.est, fill=species)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)
quartz()
ggplot(subset(sher, species=="Ambrosia.psilostchya" & tbase==2),
  aes(x=temptreat, y=gdd.est, fill=as.factor(preciptreat))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)

quartz()
ggplot(subset(sher, species=="Schizachyrium.scoparium" & tbase==2),
  aes(x=temptreat, y=gdd.est, fill=as.factor(preciptreat))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)

quartz()
ggplot(subset(sher, species=="Panicum.virgatum" & tbase==2),
  aes(x=temptreat, y=gdd.est, fill=as.factor(preciptreat))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)

##
## looking at tbase more
## need to model-select best tbase FIRST
##
findbetterway <- ddply(bace, c("plot", "species"), summarise,
       modaic=min(modaic))

bace.tbase <- merge(findbetterway, bace, all.x=TRUE)

plot(gdd.est~tbase, data=bace.tbase) # base 10 is selected, probably because errors look small when data are not standardized .....


##
## let's catapult ahead!
## and try some big models
##

gdd2 <- subset(gdd, tbase==2)
gdd2.warmonly <- subset(gdd2, preciptreat==0)

quartz()
ggplot(gdd2,
  aes(x=temptreat, y=gdd.est, fill=site)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)

mod <- lmer(gdd.est~temptreat+ (1|site/species), data=gdd2.warmonly)
