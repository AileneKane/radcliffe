#Figure/analyses of block and year differences in warming treatment effects
#By Ailene Ettinger
#Started Septembr 6, 2016
#modified for NCC manuscript February 24, 2017 
#modified for EL Reviews February 2018
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
expclimt$agtemp_mn[which(is.na(expclimt$agtemp_min) & is.na(expclimt$cantemp_min))]<-expclimt$airtemp_mean[which(is.na(expclimt$agtemp_min) & is.na(expclimt$cantemp_min))]
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
quartz(height=7,width=7)
par(mfrow=c(2,2),mai=c(1,1,.2,.2))
sites<-unique(allwarm_block$site)
#cols<-c("darkgray","darkred","darkgray","darkgray","darkgray","darkgray")#determined by the warming type: infrared=23, soil warming=24
cols<-c("black","darkred","black","black","black","black")#determined by the warming type: infrared=23, soil warming=24

shapes<-c(23,22,10,3,22,21)
#cols<-c("darkblue","lightblue","darkgreen","gray","white","lightgreen")
#colors determined by the warming type: infrared="darkgreen", soil warming="lightblue"
#allwarm_blockag<-allwarm_block[-which(allwarm_block$agwarm=="NaN"),]
#sitesag<-unique(allwarm_blockag$site)
allwarm_block$target<-as.numeric(allwarm_block$target)
allwarm_block$agwarm<-as.numeric(allwarm_block$agwarm)
#allwarm_blockag$target<-as.numeric(allwarm_blockag$target)
#allwarm_blockag$agwarm<-as.numeric(allwarm_blockag$agwarm)
plot(allwarm_block$target,allwarm_block$agwarm,pch=shapes[as.factor(allwarm_block$site)], bg=cols[as.factor(allwarm_block$site)],col=cols[as.factor(allwarm_block$site)],xlab="", ylab="Above-ground", bty="l", main="By Block", xlim=c(-0.5,6), ylim=c(-0.5,6), cex.lab=1.1, cex.axis=1.1)
#points(allwarm_stmean$target,allwarm_stmean$agwarm,bg=cols[as.factor(allwarm_stmean$site)],pch=shapes[as.numeric(as.factor(as.numeric(allwarm_stmean$site)))])

abline(a=0,b=1,lty=2)
abline(a=fixef(mab.mm)[1],b=fixef(mab.mm)[2], lty=1)
mtext("Observed warming (ºC)", side=2,line=4,adj=-5, cex=1.2)
#col=cols[as.factor(allwarm_yr$site)],
plot(allwarm_yr$target,allwarm_yr$agwarm,pch=shapes[as.factor(allwarm_yr$site)][-which(allwarm_yr$site=="exp08")], bg=cols[as.factor(allwarm_yr$site)],col=cols[as.factor(allwarm_yr$site)][-which(allwarm_yr$site=="exp08")],xlab="", ylab="Above-ground", bty="l", main="By Year", xlim=c(-0.5,6), ylim=c(-0.5,6), cex.lab=1.1, cex.axis=1.1)
abline(a=0,b=1,lty=2)
abline(a=fixef(may.mm)[1],b=fixef(may.mm)[2], lty=1)
# col=cols[as.factor(allwarm_block$site)],
plot(allwarm_block$target,allwarm_block$soilwarm,pch=shapes[as.factor(allwarm_block$site)],bg=cols[as.factor(allwarm_block$site)],col=cols[as.factor(allwarm_block$site)],xlab="", ylab="Soil", bty="l", xlim=c(-0.5,6), ylim=c(-0.5,6), cex.lab=1.1, cex.axis=1.1)

abline(a=0,b=1,lty=2)
abline(a=fixef(msb.mm)[1],b=fixef(msb.mm)[2], lty=1)

plot(allwarm_yr$target,allwarm_yr$soilwarm,pch=shapes[as.factor(allwarm_yr$site)],  bg=cols[as.factor(allwarm_yr$site)],col=cols[as.factor(allwarm_yr$site)],xlab="", ylab="Soil", bty="l", xlim=c(-0.5,6), ylim=c(-0.5,6), cex.lab=1.1, cex.axis=1.1)
abline(a=0,b=1,lty=2)
abline(a=fixef(msy.mm)[1],b=fixef(msy.mm)[2], lty=1)
legend(4.15,2.9,pch=shapes,pt.bg=cols,col=cols,legend=sites)
mtext("Target warming (ºC)", side=1,line=3,adj=-3, cex=1.2)


#Plot means across duration of study
#fit line between target and observed for soil and air for study duration
mab.m<-lm(agwarm~target, data=allwarm_stmean)
ms.m<-lm(soilwarm~target, data=allwarm_stmean)

quartz(height=6.6,width=4)
par(mfrow=c(2,1),mai=c(1,1,.2,.2))
#unique((allwarm_stmean$site))
plot(allwarm_stmean$target,allwarm_stmean$agwarm,bg=cols[as.factor(allwarm_stmean$site)],col=cols[as.factor(allwarm_stmean$site)],pch=shapes[as.factor(allwarm_stmean$site)],xlab="", ylab="Above-ground", bty="l", main="Mean Across Duration of Study", xlim=c(-0.5,6), ylim=c(-0.5,6), cex.lab=1.1, cex.axis=1.1)
mtext("Observed warming (ºC)", side=2,line=4,adj=-15, cex=1.2)
abline(a=0,b=1,lty=2)
abline(a=coef(mab.m)[1],b=coef(mab.m)[2], lty=1)

plot(allwarm_stmean$target,allwarm_stmean$soilwarm,bg=cols[as.factor(allwarm_stmean$site)],col=cols[as.factor(allwarm_stmean$site)],pch=shapes[as.factor(allwarm_stmean$site)],xlab="", ylab="Soil", bty="l", xlim=c(-0.5,6), ylim=c(-0.5,6), cex.lab=1.1, cex.axis=1.1)
abline(a=0,b=1,lty=2)
abline(a=coef(ms.m)[1],b=coef(ms.m)[2], lty=1)
legend(4.5,3,pch=shapes,pt.bg=cols,col=cols,legend=sites, cex=.8)
mtext("Target warming (ºC)", side=1,line=3,adj=.5, cex=1.2)

#try with reported warming instead of Target (to address REviewer 1's comments):
head(blockdat2)
#aggregate observed ag warming by block
ag_warm_block<-aggregate(blockdat2$ag_warm, by=list(blockdat2$site,blockdat2$block,blockdat2$reported), FUN=mean,na.rm=TRUE)

#aggregateobserved soilwarming by block 
soil_warm_block<-aggregate(blockdat2$soil_warm, by=list(blockdat2$site,blockdat2$block,blockdat2$reported), FUN=mean,na.rm=TRUE)

#aggregate observed ag warming by year
ag_warm_yr<-aggregate(blockdat2$ag_warm, by=list(blockdat2$site,blockdat2$year,blockdat2$reported), FUN=mean,na.rm=TRUE)

#aggregate soil observed warming by year
soil_warm_yr<-aggregate(blockdat2$soil_warm, by=list(blockdat2$site,blockdat2$year,blockdat2$reported), FUN=mean,na.rm=TRUE)
allwarm_block<-cbind(ag_warm_block,soil_warm_block$x)
allwarm_yr<-cbind(ag_warm_yr,soil_warm_yr$x)
colnames(allwarm_block)<-c("site","block","reported","agwarm","soilwarm")
colnames(allwarm_yr)<-c("site","year","reported","agwarm","soilwarm")
allwarm_block<-allwarm_block[order(allwarm_block$site,allwarm_block$block),]
allwarm_yr<-allwarm_yr[order(allwarm_yr$site,allwarm_yr$year),]

#fit line between reported and observed for soil and air by block and year
mab.mm<-lmer(agwarm~reported + (1|site), data=allwarm_block)
#msb<-lm(agwarm~reported, data=allwarm_block)
may.mm<-lmer(agwarm~reported + (1|site), data=allwarm_yr)
#msy<-lm(agwarm~reported, data=allwarm_yr)
msb.mm<-lmer(soilwarm~reported + (1|site), data=allwarm_block)
msy.mm<-lmer(soilwarm~reported + (1|site), data=allwarm_yr)

#add comparison of just mean across duration of study
#aggregate observed ag warming
ag_warm_stmean<-aggregate(blockdat2$ag_warm, by=list(blockdat2$site,blockdat2$reported), FUN=mean,na.rm=TRUE)

#aggregateobserved soilwarming 
soil_warm_stmean<-aggregate(blockdat2$soil_warm, by=list(blockdat2$site,blockdat2$reported), FUN=mean,na.rm=TRUE)

allwarm_stmean<-cbind(ag_warm_stmean,soil_warm_stmean$x)
colnames(allwarm_stmean)<-c("site","reported","agwarm","soilwarm")
allwarm_stmean<-allwarm_stmean[order(allwarm_stmean$site),]


#plot fitted lines, points, and 1:1 line
quartz(height=7,width=7)
par(mfrow=c(2,2),mai=c(1,1,.2,.2))
sites<-unique(allwarm_block$site)
#shapes<-c(23,24,23,23,23)#determined by the warming type: infrared=23, soil warming=24
#cols<-c("darkblue","lightblue","darkgreen","gray","white","lightgreen")#determind by site
#allwarm_blockag<-allwarm_block[-which(allwarm_block$agwarm=="NaN"),]
#sitesag<-unique(allwarm_blockag$site)
allwarm_block$reported<-as.numeric(allwarm_block$reported)
allwarm_block$agwarm<-as.numeric(allwarm_block$agwarm)
#allwarm_blockag$reported<-as.numeric(allwarm_blockag$reported)
#allwarm_blockag$agwarm<-as.numeric(allwarm_blockag$agwarm)
plot(allwarm_block$reported,allwarm_block$agwarm,pch=shapes[as.factor(allwarm_block$site)], col=cols[as.factor(allwarm_block$site)],bg=cols[as.factor(allwarm_block$site)],xlab="", ylab="Above-ground", bty="l", main="By Block", xlim=c(-0.5,6), ylim=c(-0.5,6), cex.lab=1.1, cex.axis=1.1)
#points(allwarm_stmean$reported,allwarm_stmean$agwarm,bg=cols[as.factor(allwarm_stmean$site)],pch=shapes[as.numeric(as.factor(as.numeric(allwarm_stmean$site)))])

abline(a=0,b=1,lty=2)
abline(a=fixef(mab.mm)[1],b=fixef(mab.mm)[2], lty=1)
mtext("Observed warming (ºC)", side=2,line=4,adj=-8, cex=1.2)

plot(allwarm_yr$reported,allwarm_yr$agwarm,pch=shapes[as.factor(allwarm_yr$site)], col=cols[as.factor(allwarm_block$site)],bg=cols[as.factor(allwarm_yr$site)],xlab="", ylab="Above-ground", bty="l", main="By Year", xlim=c(-0.5,6), ylim=c(-0.5,6), cex.lab=1.1, cex.axis=1.1)
abline(a=0,b=1,lty=2)
abline(a=fixef(may.mm)[1],b=fixef(may.mm)[2], lty=1)

plot(allwarm_block$reported,allwarm_block$soilwarm,pch=shapes[as.factor(allwarm_block$site)],col=cols[as.factor(allwarm_yr$site)],bg=cols[as.factor(allwarm_block$site)],xlab="", ylab="Soil", bty="l", xlim=c(-0.5,6), ylim=c(-0.5,6), cex.lab=1.1, cex.axis=1.1)

abline(a=0,b=1,lty=2)
abline(a=fixef(msb.mm)[1],b=fixef(msb.mm)[2], lty=1)

plot(allwarm_yr$reported,allwarm_yr$soilwarm,pch=shapes[as.factor(allwarm_yr$site)], col=cols[as.factor(allwarm_yr$site)],bg=cols[as.factor(allwarm_yr$site)],xlab="", ylab="Soil", bty="l", xlim=c(-0.5,6), ylim=c(-0.5,6), cex.lab=1.1, cex.axis=1.1)
abline(a=0,b=1,lty=2)
abline(a=fixef(msy.mm)[1],b=fixef(msy.mm)[2], lty=1)
legend(4.15,2.9,pch=shapes,col=cols,pt.bg=cols,legend=sites)
mtext("Reported warming (ºC)", side=1,line=3,adj=-5, cex=1.2)


#Plot means across duration of study
#fit line between reported and observed for soil and air for study duration
mab.m<-lm(agwarm~reported, data=allwarm_stmean)
ms.m<-lm(soilwarm~reported, data=allwarm_stmean)

quartz(height=6.6,width=4)
par(mfrow=c(2,1),mai=c(1,1,.2,.2))
#unique((allwarm_stmean$site))
plot(allwarm_stmean$reported,allwarm_stmean$agwarm,bg=cols[as.factor(allwarm_stmean$site)],pch=shapes[as.factor(allwarm_stmean$site)],xlab="", ylab="Above-ground", bty="l", main="Mean Across Duration of Study", xlim=c(-0.5,6), ylim=c(-0.5,6), cex.lab=1.1, cex.axis=1.1)
mtext("Observed warming (ºC)", side=2,line=4,adj=-15, cex=1.2)
abline(a=0,b=1,lty=2)
abline(a=coef(mab.m)[1],b=coef(mab.m)[2], lty=1)

plot(allwarm_stmean$reported,allwarm_stmean$soilwarm,bg=cols[as.factor(allwarm_stmean$site)],pch=shapes[as.factor(allwarm_stmean$site)],xlab="", ylab="Soil", bty="l", xlim=c(-0.5,6), ylim=c(-0.5,6), cex.lab=1.1, cex.axis=1.1)
abline(a=0,b=1,lty=2)
abline(a=coef(ms.m)[1],b=coef(ms.m)[2], lty=1)
legend(4.5,3,pch=shapes,pt.bg=cols,legend=sites, cex=.8)
mtext("Reported warming (ºC)", side=1,line=3,adj=.5, cex=1.2)

#Reviewer asked for mean or median difference among plots with same target treatment, 
agwarm_min<-aggregate(allwarm_block$agwarm, by=list(allwarm_block$site,allwarm_block$target),min, na.rm=TRUE)
agwarm_max<-aggregate(allwarm_block$agwarm, by=list(allwarm_block$site,allwarm_block$target),max, na.rm=TRUE)
meandiff<-mean(agwarm_max$x[1:8]-agwarm_min$x[1:8])
meddiff<-median(agwarm_max$x[1:8]-agwarm_min$x[1:8])
