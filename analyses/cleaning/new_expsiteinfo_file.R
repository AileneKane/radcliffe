# ------------------------------------------
# Create detailed file with treatment information
# A. Ettinger, aettinger@fas.harvard.edu
# Description:Code to make more useful expsiteinfo.csv file, 
# with all blocks, plots, treatment levels, reported observed 
# temperature, etc included in it

setwd("~/git/radcliffe")
rm(list=ls()) 
options(stringsAsFactors=FALSE)

library(tidyr)
library(dplyr)
# start with expsiteinfo.csv  file:
expsite<-read.csv("analyses/expsiteinfo.csv", header=TRUE)
expsite2<-subset(expsite,select=c("Site","FirstAuthor","warming_type","temptreat_1","temptreat_2","temptreat_3","temptreat_4","temptreat_5","temptreat_6","temptreat_7","temptreat_8","temptreat_9","temptreat_units","preciptreat_1","preciptreat_.1","preciptreat_units"))
expsite3<-subset(expsite,select=c("Site","FirstAuthor","temptreat_1_reported","temptreat_2_reported","temptreat_3_reported","temptreat_4_reported","temptreat_5_reported","temptreat_6_reported","temptreat_7_reported","temptreat_8_reported","temptreat_9_reported"))
exptreats<-gather(expsite2,key=treat,value=target,temptreat_1,temptreat_2,temptreat_3,temptreat_4,temptreat_5,temptreat_6,temptreat_7,temptreat_8,temptreat_9,na.rm=TRUE)
exptreats2<-gather(expsite3,key=treat,value=reported,temptreat_1_reported,temptreat_2_reported,temptreat_3_reported,temptreat_4_reported,temptreat_5_reported,temptreat_6_reported,temptreat_7_reported,temptreat_8_reported,temptreat_9_reported,na.rm=TRUE)
exptreats2$treat<-substr(exptreats2$treat,1,11)
exptreats3<-left_join(exptreats,exptreats2, by = c("Site","FirstAuthor","treat"))
##add plot to this, from exppheno file
expphen<-read.csv("analyses/exppheno.csv", header=T)
expphen$site.block.plot<-paste(expphen$site,expphen$block,expphen$plot,sep=".")
expplots <- expphen %>% # start with the data frame
  distinct(site.block.plot,.keep_all = TRUE) %>% # establishing grouping variables
  select(site, block, plot,site.block.plot)
expplots  <- expplots [apply(expplots , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
####check that plots match plots in expclim file
expclim<-read.csv("analyses/expclim.csv", header=T)
expclim$site.block.plot<-paste(expclim$site,expclim$block,expclim$plot,sep=".")
expclimplots <- expclim %>% # start with the data frame
  distinct(site.block.plot,.keep_all = TRUE) %>% # establishing grouping variables
  select(site, block, plot,site.block.plot)
  expclimplots  <- expclimplots [apply(expclimplots , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#check for missing/nonmatching site/block/plots between these two files
expphenplots_nomatch<-expplots[which(is.na(match(expplots$site.block.plot,expclimplots$site.block.plot))),]
##now add plot to this, from the treats file
treats<-read.csv("analyses/treats.csv", header=T)
treats