#Figure/analyses of block and year differences in warming treatment effects
#By Ailene Ettinger
#Started Septembr 6, 2016
#modified for NCC manuscript February 24, 2017 
#block and year figure with line showing fitted relationship between target versus observed

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(plyr)
library(dplyr)
library(car)
library(lme4)

setwd("~/git/radcliffe/Analyses")
#Aggregate above-ground observed warming by block (difference between treatment and control within each block)
#to do this, i need agtemps by site, block,plot, doy, and year
expclim<-read.csv("expclim.csv", header=T)
treats<-read.csv("treats_detail.csv", header=T)

#want to compare mean, dtr, and variances of min and max temperatures in control plots and warmed plots in each study
expclim2<-full_join(treats,expclim, by=c("site", "block", "plot","temptreat","preciptreat"), match="first")
expclim2$styear<-NA#start by giving all studies year 1 (exp2 and exp8 had only 1 year each),then adjust each study by hand
#make a column for styear (study year, as opposed to calendar year)
sites<-unique(expclim2$site)
for (i in 1:length(sites)){
  sitedat<-expclim2[expclim2$site==sites[i],]
  styears<-unique(sitedat$year)
  #print(styears)
  for (j in 1:length(styears)){
    expclim2$styear[expclim2$site==sites[i] & expclim2$year==styears[j]]<-j
  }
}
expclimt<-expclim2[which(expclim2$preciptreat==0|is.na(expclim2$preciptreat)),]
expclimt$agtemp_min<-expclimt$airtemp_min
expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$cantemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$airtemp_min) & !is.na(expclimt$cantemp_min)),]$cantemp_min
expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$surftemp_min
expclimt$agtemp_max<-expclimt$airtemp_max
expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$cantemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$airtemp_max) & !is.na(expclimt$cantemp_max)),]$cantemp_max
expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$surftemp_max
#remove site 2 (chuine) because these are not real temp measurements
#remove site 5 (cleland) because only soil moisutre measured (no temp)
expclimt<-expclimt[-which(expclimt$site=="exp02"|expclimt$site=="exp05"),]

expclimt$agtemp_mn<-(expclimt$agtemp_max+expclimt$agtemp_min)/2
blockdat<-expclimt[which(!is.na(expclimt$block)),]

#add column for control plot temp
controls<-blockdat[which(blockdat$temptreat==0 |blockdat$temptreat=="ambient"),]
controls2<-subset(controls,select=c(site,block,year,styear,doy,temptreat,agtemp_mn,soiltemp1_mean))
#exps8 has both types of controls. use structural control as reference for these
controls2<-controls2[-which(controls2$site=="exp08" & controls2$temptreat=="ambient"),]
controls3<-controls2[,-(which(colnames(controls2)=="temptreat"))]
colnames(controls3)[6:7]<-c("agtemp_mn_cont","soiltemp1_mn_cont")
blockdat2<-full_join(controls3,blockdat, by=c("site", "block", "year","styear","doy"), match="first")
blockdat2$ag_warm<-blockdat2$agtemp_mn-blockdat2$agtemp_mn_cont
blockdat2$soil_warm<-blockdat2$soiltemp1_mean-blockdat2$soiltemp1_mn_cont

#aggregate observed ag warming by block
ag_warm_block<-aggregate(blockdat2$ag_warm, by=list(blockdat2$site,blockdat2$block,blockdat2$target), FUN=mean,na.rm=TRUE)

#aggregateobserved soilwarming by block 
soil_warm_block<-aggregate(blockdat2$soil_warm, by=list(blockdat2$site,blockdat2$block,blockdat2$target), FUN=mean,na.rm=TRUE)

#aggregate observed ag warming by year
ag_warm_yr<-aggregate(blockdat2$ag_warm, by=list(blockdat2$site,blockdat2$year,blockdat2$target), FUN=mean,na.rm=TRUE)

#aggregate soil observed warming by year
soil_warm_yr<-aggregate(blockdat2$soil_warm, by=list(blockdat2$site,blockdat2$year,blockdat2$target), FUN=mean,na.rm=TRUE)
allwarm_block<-cbind(ag_warm_block,soil_warm_block$x)
allwarm_yr<-cbind(ag_warm_yr,soil_warm_yr$x)
colnames(allwarm_block)<-c("site","block","target","agwarm","soilwarm")
colnames(allwarm_yr)<-c("site","year","target","agwarm","soilwarm")
allwarm_block<-allwarm_block[order(allwarm_block$site,allwarm_block$block),]
allwarm_yr<-allwarm_yr[order(allwarm_yr$site,allwarm_yr$year),]

#fit line between target and observed for soil and air by block and year
mab.mm<-lmer(agwarm~target + (1|site), data=allwarm_block)
#msb<-lm(agwarm~target, data=allwarm_block)
may.mm<-lmer(agwarm~target + (1|site), data=allwarm_yr)
#msy<-lm(agwarm~target, data=allwarm_yr)
msb.mm<-lmer(soilwarm~target + (1|site), data=allwarm_block)
msy.mm<-lmer(soilwarm~target + (1|site), data=allwarm_yr)

#add comparison of just mean across duration of study
#aggregate observed ag warming
ag_warm_stmean<-aggregate(blockdat2$ag_warm, by=list(blockdat2$site,blockdat2$target), FUN=mean,na.rm=TRUE)

#aggregateobserved soilwarming 
soil_warm_stmean<-aggregate(blockdat2$soil_warm, by=list(blockdat2$site,blockdat2$target), FUN=mean,na.rm=TRUE)

allwarm_stmean<-cbind(ag_warm_stmean,soil_warm_stmean$x)
colnames(allwarm_stmean)<-c("site","target","agwarm","soilwarm")
allwarm_stmean<-allwarm_stmean[order(allwarm_stmean$site),]


#plot fitted lines, points, and 1:1 line
quartz(height=6,width=8)
par(mfrow=c(2,2),mai=c(1,1,.2,.2))
sites<-unique(allwarm_block$site)
allwarm_blockag<-allwarm_block[-which(allwarm_block$agwarm=="NaN"),]
sitesag<-unique(allwarm_blockag$site)
allwarm_block$target<-as.numeric(allwarm_block$target)
allwarm_block$agwarm<-as.numeric(allwarm_block$agwarm)
#allwarm_blockag$target<-as.numeric(allwarm_blockag$target)
#allwarm_blockag$agwarm<-as.numeric(allwarm_blockag$agwarm)
plot(allwarm_block$target,allwarm_block$agwarm,pch=as.numeric(as.factor(as.numeric(allwarm_block$site)))+20, col="black",bg="gray",xlab="", ylab="Above-ground", bty="l", main="By Block", xlim=c(0,6), ylim=c(0,6))
points(allwarm_stmean$target,allwarm_stmean$agwarm,bg="black",pch=as.numeric(as.factor(as.numeric(allwarm_stmean$site)))+20)

abline(a=0,b=1,lty=2)
abline(a=fixef(mab.mm)[1],b=fixef(mab.mm)[2], lty=1)
mtext("Observed warming (C)", side=2,line=4,adj=8, cex=1.2)

plot(allwarm_yr$target,allwarm_yr$agwarm,pch=as.numeric(as.factor(as.numeric(allwarm_block$site)))+20, col="black",bg="gray",xlab="", ylab="Above-ground", bty="l", main="By Year", xlim=c(0,6), ylim=c(0,6))
abline(a=0,b=1,lty=2)
abline(a=fixef(may.mm)[1],b=fixef(may.mm)[2], lty=1)

plot(allwarm_block$target,allwarm_block$soilwarm,pch=as.numeric(as.factor(as.numeric(allwarm_block$site)))+20, col="black",bg="gray",xlab="", ylab="Soil", bty="l", xlim=c(0,6), ylim=c(0,6))

abline(a=0,b=1,lty=2)
abline(a=fixef(msb.mm)[1],b=fixef(msb.mm)[2], lty=1)

plot(allwarm_yr$target,allwarm_yr$soilwarm,pch=as.numeric(as.factor(as.numeric(allwarm_block$site)))+20, col="black",bg="gray",xlab="", ylab="Soil", bty="l", xlim=c(0,6), ylim=c(0,6))
abline(a=0,b=1,lty=2)
abline(a=fixef(msy.mm)[1],b=fixef(msy.mm)[2], lty=1)
legend(4.5,3.5,pch=c(21,22,23,24),pt.bg="gray",legend=c("exp01","exp08","exp09","exp12"))
mtext("Target warming (C)", side=1,line=3,adj=-1.5, cex=1.2)


#Plot means across duration of study
#fit line between target and observed for soil and air for study duration
mab.m<-lm(agwarm~target, data=allwarm_stmean)
ms.m<-lm(soilwarm~target, data=allwarm_stmean)

quartz(height=6,width=6)
par(mfrow=c(2,1),mai=c(1,1,.2,.2))

plot(allwarm_stmean$target,allwarm_stmean$agwarm,bg="black",pch=as.numeric(as.factor(as.numeric(allwarm_stmean$site)))+20,xlab="", ylab="Above-ground", bty="l", main="Mean Across Duration of Study", xlim=c(0,6), ylim=c(0,6))
mtext("Observed warming (C)", side=2,line=4,adj=8, cex=1.2)
abline(a=0,b=1,lty=2)
abline(a=coef(mab.m)[1],b=coef(mab.m)[2], lty=1)

plot(allwarm_stmean$target,allwarm_stmean$soilwarm,bg="black",pch=as.numeric(as.factor(as.numeric(allwarm_stmean$site)))+20,xlab="", ylab="Soil", bty="l", xlim=c(0,6), ylim=c(0,6))
abline(a=0,b=1,lty=2)
abline(a=coef(ms.m)[1],b=coef(ms.m)[2], lty=1)
legend(4.5,3.5,pch=c(21,22,23,24),pt.bg="black",legend=c("exp01","exp08","exp09","exp12"))
mtext("Target warming (C)", side=1,line=3,adj=.5, cex=1.2)

