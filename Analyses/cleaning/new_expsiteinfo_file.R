# ------------------------------------------
# Create detailed file with treatment information
# A. Ettinger, aettinger@fas.harvard.edu
# Description:Code to make more useful treats_detailed.csv file 
# with all blocks, plots, treatment levels, reported observed 
# temperature, etc included in it

setwd("~/git/radcliffe")
rm(list=ls()) 
options(stringsAsFactors=FALSE)

library(tidyr)
library(dplyr)
# start with expsiteinfo.csv  file:
expsite<-read.csv("analyses/expsiteinfo.csv", header=TRUE)
expsite2<-subset(expsite,select=c("DatasetID","FirstAuthor","warming_type","temptreat_1","temptreat_2","temptreat_3","temptreat_4","temptreat_5","temptreat_6","temptreat_7","temptreat_8","temptreat_9","temptreat_units"))
expsite3<-subset(expsite,select=c("DatasetID","FirstAuthor","temptreat_1_reported","temptreat_2_reported","temptreat_3_reported","temptreat_4_reported","temptreat_5_reported","temptreat_6_reported","temptreat_7_reported","temptreat_8_reported","temptreat_9_reported"))
exptreats<-gather(expsite2,key=treat,value=target,temptreat_1,temptreat_2,temptreat_3,temptreat_4,temptreat_5,temptreat_6,temptreat_7,temptreat_8,temptreat_9,na.rm=TRUE)#temperature treatments
exptreats2<-gather(expsite3,key=treat,value=reported,temptreat_1_reported,temptreat_2_reported,temptreat_3_reported,temptreat_4_reported,temptreat_5_reported,temptreat_6_reported,temptreat_7_reported,temptreat_8_reported,temptreat_9_reported,na.rm=TRUE)#reported temperature
exptreats2$treat<-substr(exptreats2$treat,1,11)
exptreats3<-left_join(exptreats,exptreats2, by = c("DatasetID","FirstAuthor","treat"))
exptreats3$temptreat<-substr(exptreats3$treat,11,11)

#now add in precip treatment
expsite4<-subset(expsite,select=c("DatasetID","FirstAuthor","preciptreat_1","preciptreat_.1","preciptreat_units"))
exptreats4<-gather(expsite4,key=ptreat,value=preciptreat_amt,preciptreat_1,preciptreat_.1,na.rm=TRUE)#reported temperature
exptreats4$preciptreat<-substr(exptreats4$ptreat,13,nchar(exptreats4$ptreat))
exptreats5<-left_join(exptreats3,exptreats4, by = c("DatasetID","FirstAuthor"))
exptreats5[which(exptreats5$preciptreat==".1"),]$preciptreat<-"-1"

##add block and plot to this, from expclim file
expclim<-read.csv("analyses/expclim.csv", header=T)
expclim$siteblockplot<-paste(expclim$site,expclim$block,expclim$plot,sep="")
expclimplots <- expclim %>% # start with the data frame
  distinct(siteblockplot,.keep_all = TRUE) %>% # establishing grouping variables
  select(site, block, plot,siteblockplot,temptreat,preciptreat)
colnames(expclimplots)[1]<-"DatasetID"
##now merge this with exptreats5 to make new treats file with more detail:
exptreats_all<-left_join(expclimplots,exptreats5, by = c("DatasetID","temptreat","preciptreat"))
exptreats_detail<-subset(exptreats_all,select=c(select=c("DatasetID","block","plot","temptreat","target","reported","temptreat_units","preciptreat","preciptreat_amt","preciptreat_units")))
head(exptreats_detail)
write.csv(exptreats_detail,"analyses/treats_detail.csv",row.names=FALSE, eol="\r\n")  
  
###Check that expphen blocks/plots march expclim blocks/plots:
expphen<-read.csv("analyses/exppheno.csv", header=T)
expphen$site.block.plot<-paste(expphen$site,expphen$block,expphen$plot,sep=".")
expplots <- expphen %>% # start with the data frame
  distinct(site.block.plot,.keep_all = TRUE) %>% # establishing grouping variables
  select(site, block, plot,site.block.plot)
expplots  <- expplots [apply(expplots , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#check for missing/nonmatching site/block/plots between these two files
expphenplots_nomatch<-expplots[which(is.na(match(expplots$site.block.plot,expclimplots$site.block.plot))),]
##these are all fine- there are some plots for which there are phenology data but no climate data were collected- bace block 0 and force plots "E"
expphenplots_nomatch2<-expclimplots[which(is.na(match(expclimplots$site.block.plot,expplots$site.block.plot))),]
#for some reason, there are a few plots in the cleland dataset in which phenology data were not collected, but climate data were
