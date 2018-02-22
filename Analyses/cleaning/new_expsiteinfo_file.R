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
exptreats<-gather(expsite2,key=treat,value=target,temptreat_1,temptreat_2,temptreat_3,temptreat_4,temptreat_5,temptreat_6,temptreat_7,temptreat_8,temptreat_9,na.rm=FALSE)#temperature treatments
exptreats2<-gather(expsite3,key=treat,value=reported,temptreat_1_reported,temptreat_2_reported,temptreat_3_reported,temptreat_4_reported,temptreat_5_reported,temptreat_6_reported,temptreat_7_reported,temptreat_8_reported,temptreat_9_reported,na.rm=FALSE)#reported temperature
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
  dplyr::select(site, block, plot,siteblockplot,temptreat,preciptreat)
colnames(expclimplots)[1]<-"DatasetID"
expclimplots$preciptreat<-as.character(expclimplots$preciptreat)

##now merge this with exptreats5 to make new treats file with more detail:
exptreats_all<-left_join(expclimplots,exptreats5, by = c("DatasetID","temptreat","preciptreat"))
exptreats_detail<-subset(exptreats_all,select=c(select=c("DatasetID","block","plot","temptreat","target","reported","temptreat_units","preciptreat","preciptreat_amt","preciptreat_units")))
head(exptreats_detail)
exptreats_detail[which(exptreats_detail$DatasetID=="exp09" & exptreats_detail$temptreat=="1"),]$target<-2
exptreats_detail[which(exptreats_detail$DatasetID=="exp09" & exptreats_detail$temptreat=="1"),]$reported<-2.3
exptreats_detail[which(exptreats_detail$DatasetID=="exp09" & exptreats_detail$temptreat=="1"),]$temptreat_units<-"C"
exptreats_detail[which(exptreats_detail$DatasetID=="exp09" & exptreats_detail$preciptreat=="1"),]$preciptreat_amt<-120
exptreats_detail[which(exptreats_detail$DatasetID=="exp09" & exptreats_detail$preciptreat=="1"),]$preciptreat_units<-"perchistmean"
exptreats_detail[which(exptreats_detail$DatasetID=="exp01" & exptreats_detail$temptreat=="1"),]$target<-1
exptreats_detail[which(exptreats_detail$DatasetID=="exp01" & exptreats_detail$temptreat=="2"),]$target<-2.7
exptreats_detail[which(exptreats_detail$DatasetID=="exp01" & exptreats_detail$temptreat=="3"),]$target<-4
exptreats_detail[which(exptreats_detail$DatasetID=="exp01" & exptreats_detail$temptreat>0),]$temptreat_units<-"C"
exptreats_detail[which(exptreats_detail$DatasetID=="exp01" & exptreats_detail$preciptreat=="1"),]$preciptreat_amt<-150
exptreats_detail[which(exptreats_detail$DatasetID=="exp01" & exptreats_detail$preciptreat=="-1"),]$preciptreat_amt<-50
exptreats_detail[which(exptreats_detail$DatasetID=="exp01" & exptreats_detail$preciptreat=="1"),]$preciptreat_units<-"perc"
exptreats_detail[which(exptreats_detail$DatasetID=="exp01" & exptreats_detail$preciptreat=="-1"),]$preciptreat_units<-"perc"
exptreats_detail[which(exptreats_detail$DatasetID=="exp12" & exptreats_detail$temptreat=="1"),]$target<-4
exptreats_detail[which(exptreats_detail$DatasetID=="exp12" & exptreats_detail$temptreat=="1"),]$temptreat_units<-"C"
exptreats_detail[which(exptreats_detail$DatasetID=="exp12" & exptreats_detail$temptreat=="1"),]$reported<-4.17
exptreats_detail[which(exptreats_detail$DatasetID=="exp12" & exptreats_detail$preciptreat=="1"),]$preciptreat_amt<-200
exptreats_detail[which(exptreats_detail$DatasetID=="exp12" & exptreats_detail$preciptreat=="1"),]$preciptreat_units<-"perc"
exptreats_detail[which(exptreats_detail$DatasetID=="exp02" & exptreats_detail$temptreat=="1"),]$target<-1.5
exptreats_detail[which(exptreats_detail$DatasetID=="exp02" & exptreats_detail$temptreat=="1"),]$reported<-1.5
exptreats_detail[which(exptreats_detail$DatasetID=="exp02" & exptreats_detail$temptreat=="1"),]$temptreat_units<-"C"
exptreats_detail[which(exptreats_detail$DatasetID=="exp02" & exptreats_detail$temptreat=="2"),]$target<-3
exptreats_detail[which(exptreats_detail$DatasetID=="exp02" & exptreats_detail$temptreat=="2"),]$reported<-3
exptreats_detail[which(exptreats_detail$DatasetID=="exp02" & exptreats_detail$temptreat=="2"),]$temptreat_units<-"C"
exptreats_detail[which(exptreats_detail$DatasetID=="exp02" & exptreats_detail$preciptreat=="-1"),]$preciptreat_amt<-70
exptreats_detail[which(exptreats_detail$DatasetID=="exp02" & exptreats_detail$preciptreat=="-1"),]$preciptreat_units<-"perc"

exptreats_detail[which(exptreats_detail$DatasetID=="exp05" & exptreats_detail$temptreat=="1"),]$target<-1.5
exptreats_detail[which(exptreats_detail$DatasetID=="exp05" & exptreats_detail$temptreat=="1"),]$reported<-1
exptreats_detail[which(exptreats_detail$DatasetID=="exp05" & exptreats_detail$temptreat=="1"),]$temptreat_units<-"C"
exptreats_detail[which(exptreats_detail$DatasetID=="exp05" & exptreats_detail$preciptreat=="1"),]$preciptreat_amt<-150
exptreats_detail[which(exptreats_detail$DatasetID=="exp05" & exptreats_detail$preciptreat=="1"),]$preciptreat_units<-"perc"
colnames(exptreats_detail)[1]<-"site"

write.csv(exptreats_detail,"analyses/treats_detail.csv",row.names=FALSE, eol="\r\n")  
  
###Check that expphen blocks/plots match expclim blocks/plots:
expphen<-read.csv("analyses/exppheno.csv", header=T)
expphen$site.block.plot<-paste(expphen$site,expphen$block,expphen$plot,sep=".")
expclimplots$site.block.plot<-paste(expclimplots$DatasetID,expclimplots$block,expclimplots$plot,sep=".")
expphen$site.block.plot<-paste(expphen$site,expphen$block,expphen$plot,sep=".")

expplots <- expphen %>% # start with the data frame
  distinct(site.block.plot,.keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(site, block, plot,site.block.plot)
#expplots  <- expplots [apply(expplots , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#check for missing/nonmatching site/block/plots between these two files
expphenplots_nomatch<-expplots[which(is.na(match(expplots$site.block.plot,expclimplots$site.block.plot))),]
##these are all fine- there are some plots for which there are phenology data but no climate data were collected- bace block 0 and force plots "E"
expphenplots_nomatch2<-expclimplots[which(is.na(match(expclimplots$site.block.plot,expplots$site.block.plot))),]
#these are all fine, there are a few plots (33,34,35,36) in the cleland dataset and one plot in the clarkduke dataset (G08) in which climate data were collected, but no phenology data were

##now need to check with effective warming data...for some reason that is not lining up.
###Check that expphen blocks/plots match expclim blocks/plots:
effwarm<-read.csv("analyses/EffectiveWarming_Plot.csv", header=T)
effwarm$site.block.plot<-paste(effwarm$site,effwarm$block,effwarm$plot,sep=".")

effwarmplots <- effwarm %>% # start with the data frame
  distinct(site.block.plot,.keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(site, block, plot,site.block.plot)
#check for missing/nonmatching site/block/plots between effective warming and expphen
exppheneffwarmplots_nomatch<-expplots[which(is.na(match(expplots$site.block.plot,effwarm$site.block.plot))),]
##misalignment with blocks in exp12 now- effective warming is off- there are some plots for which there are phenology data but no climate data were collected- bace block 0 and force plots "E"
effwarmplots_nomatch2<-effwarmplots[which(is.na(match(effwarmplots$site.block.plot,expplots$site.block.plot))),]
#again, there is a misalignment with blocks in exp12
