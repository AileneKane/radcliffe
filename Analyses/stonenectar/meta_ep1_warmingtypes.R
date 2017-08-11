## Started on 9 August 2017 ##
## Taken from See timingmeta_analysis.R in /Documents/R/NCEAS/Phenology/WorkingGroup/Experiments/Meta (Lizzie's computer) ##
## Need to run previous code in aforementioned file to get this to run ##

## Getting estimates of temperature sensitivities from different warming methods ##

##
##
## One more model of warming types
phenffdtmeanmWarmType <- phenffdtmeanm

phenffdtmeanmWarmType$warmingStudytype[
    phenffdtmeanmWarmType$warmingStudytype=="chamber and electric heater"|
    phenffdtmeanmWarmType$warmingStudytype=="greenhouse chamber & heating cables (soil)"|
    phenffdtmeanmWarmType$warmingStudytype=="move inelevation"|
    phenffdtmeanmWarmType$warmingStudytype=="soilwarming"|
   phenffdtmeanmWarmType$warmingStudytype=="nightcover"] <- "other"

phenffdtmeanmWarmType$warmingStudytype[    phenffdtmeanmWarmType$warmingStudytype=="heating cables"|
    phenffdtmeanmWarmType$warmingStudytype=="heating lamps, heating cables (soil)"] <- "Irabovecanopy" 

# make sure minimum sites and species per type
phenagg <- aggregate(phenffdtmeanmWarmType["sens"], phenffdtmeanmWarmType[c("site", "warmingStudytype", "phentype")], FUN=length)
subset(phenagg, phentype=="flo")
subset(phenagg, phentype=="veg")
# Note: probably enough data for nightcover alone for veg but not flowers, but think it is better to be consistent

onemoreWarmTypemodelf <- lme(sens~warmingStudytype, random=~1|site/latbi,
    data=subset(phenffdtmeanmWarmType, phentype=="flo"))

onemoreWarmTypemodelv <- lme(sens~warmingStudytype, random=~1|site/latbi,
    data=subset(phenffdtmeanmWarmType, phentype=="veg"))

anova(onemoreWarmTypemodelf)
anova(onemoreWarmTypemodelv)

summary(onemoreWarmTypemodelf)
summary(onemoreWarmTypemodelv)


