#Figure/analyses of block and year differences in warming treatment effects
#By Ailene Ettinger
#Started Septembr 6, 2016
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(plyr)
library(dplyr)
library(car)
library(lme4)

setwd("~/git/radcliffe/Analyses")
expclim<-read.csv("expclim.csv", header=T)
head(expclim)
blockdat<-expclim[which(!is.na(expclim$block)),]
blockmns<-tapply(blockdat$soiltemp1_mean,list(blockdat$site,blockdat$block,blockdat$temptreat), mean, na.rm=TRUE)
blockmns<-blockmns[which(rownames(blockmns)=="bace"|rownames(blockmns)=="farnsworth"|rownames(blockmns)=="force"),,]
blockdat$site<-factor(blockdat$site)
blockdat$block<-factor(blockdat$block)
controls<-blockmns[,1:6,1]
controlmns<-rowMeans(controls,na.rm=T)
treat1<-blockmns[,1:6,2]
treat2<-blockmns[,1:6,3]
treat3<-blockmns[,1:6,4]
#Difference between treatments and mean control
warm1<-treat1-controls
warm2<-treat2-controls
warm3<-treat3-controls
#Now by year instead of by block
yearmns<-tapply(blockdat$soiltemp1_mean,list(blockdat$site,blockdat$year,blockdat$temptreat), mean, na.rm=TRUE)
yearmns<-yearmns[which(rownames(yearmns)=="bace"|rownames(yearmns)=="farnsworth"|rownames(yearmns)=="force"),,]
yearcontrols<-yearmns[,,1]
yearcontrols<-yearcontrols[, colSums(is.na(yearcontrols)) != nrow(yearcontrols)]#remove columns that are all NA
yeartreat1<-yearmns[,,2]
yeartreat1<-yeartreat1[, colSums(is.na(yeartreat1)) != nrow(yeartreat1)]#remove columns that are all NA
yeartreat2<-yearmns[,,3]
yeartreat2<-yeartreat2[, colSums(is.na(yeartreat2)) != nrow(yeartreat2)]#remove columns that are all NA
yeartreat3<-yearmns[,,4]
yeartreat3<-yeartreat3[, colSums(is.na(yeartreat3)) != nrow(yeartreat3)]#remove columns that are all NA
#Difference between treatments and mean control
yearwarm1<-yeartreat1-yearcontrols
yearwarm2<-yeartreat2-yearcontrols[,3:6]
yearwarm3<-yeartreat3-yearcontrols[,3:6]

#target warming
treats<-read.csv("expsiteinfo.csv", header=T)
target1<-treats[treats$DatasetID=="bace"|treats$DatasetID=="farnsworth"|treats$DatasetID=="force",26:28]
rownames(target1)<-treats[as.numeric(rownames(target1)),]$DatasetID

#plot difference between warmed and control, by block and year
quartz(height=7,width=10)
par(mfrow=c(1,2))
plot(c(target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1),c(warm1[,1],warm1[,2],warm1[,3],warm1[,4],warm1[,5],warm1[,6]),pch=rep(c(21,22,24), times=6),col="black",xlab="Target warming (C)", ylab="Observed soil warming (C)", bty="l", main="By Block", xlim=c(0,6), ylim=c(0,6))
points(c(target1$temptreat_2,target1$temptreat_2,target1$temptreat_2,target1$temptreat_2,target1$temptreat_2,target1$temptreat_2),c(warm2[,1],warm2[,2],warm2[,3],warm2[,4],warm2[,5],warm2[,6]))
points(c(target1$temptreat_3,target1$temptreat_3,target1$temptreat_3,target1$temptreat_3,target1$temptreat_3,target1$temptreat_3),c(warm3[,1],warm3[,2],warm3[,3],warm3[,4],warm3[,5],warm3[,6]))
legend(1,6,pch=c(21,22,24),legend=rownames(target1),bty="n")
plot(c(target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1),c(yearwarm1[,1],yearwarm1[,2],yearwarm1[,3],yearwarm1[,4],yearwarm1[,5],yearwarm1[,6]),pch=rep(c(21,22,24), times=6),col="black",xlab="Target warming (C)", ylab="Observed soil warming (C)", bty="l", main="By Year",xlim=c(0,6), ylim=c(0,6))
points(c(target1$temptreat_2,target1$temptreat_2,target1$temptreat_2,target1$temptreat_2),c(yearwarm2[,1],yearwarm2[,2],yearwarm2[,3],yearwarm2[,4]),pch=rep(c(21,22,24), times=4))
points(c(target1$temptreat_3,target1$temptreat_3,target1$temptreat_3,target1$temptreat_3),c(yearwarm3[,1],yearwarm3[,2],yearwarm3[,3],yearwarm3[,4]),pch=rep(c(21,22,24), times=4))
#Try for abovegroun temp
blockdat$agtemp_mean<-NA
blockdat$agtemp_mean<-(blockdat$airtemp_min+blockdat$airtemp_max)/2
blockdat[which(!is.na(blockdat$cantemp_min)),]$agtemp_mean<-(blockdat[which(!is.na(blockdat$cantemp_min)),]$cantemp_max+blockdat[which(!is.na(blockdat$cantemp_min)),]$cantemp_min)/2
blockdat[which(!is.na(blockdat$surftemp_min)),]$agtemp_mean<-(blockdat[which(!is.na(blockdat$surftemp_min)),]$surftemp_max+blockdat[which(!is.na(blockdat$surftemp_min)),]$surftemp_min)/2
agblockmns<-tapply(blockdat$agtemp_mean,list(blockdat$site,blockdat$block,blockdat$temptreat), mean, na.rm=TRUE)
agblockmns<-agblockmns[which(rownames(agblockmns)=="bace"|rownames(agblockmns)=="force"),,]
agcontrols<-agblockmns[,1:4,1]
agtreat1<-agblockmns[,1:4,2]
agtreat2<-agblockmns[,1:4,3]
agtreat3<-agblockmns[,1:4,4]
#Difference between treatments and mean control
agwarm1<-agtreat1-agcontrols
agwarm2<-agtreat2-agcontrols
agwarm3<-agtreat3-agcontrols
#Now by year instead of by block
agyearmns<-tapply(blockdat$agtemp_mean,list(blockdat$site,blockdat$year,blockdat$temptreat), mean, na.rm=TRUE)
agyearmns<-agyearmns[which(rownames(agyearmns)=="bace"|rownames(agyearmns)=="force"),,]
agyearcontrols<-agyearmns[,,1]
agyearcontrols<-agyearcontrols[, colSums(is.na(agyearcontrols)) != nrow(agyearcontrols)]#remove columns that are all NA
agyeartreat1<-agyearmns[,,2]
agyeartreat1<-agyeartreat1[, colSums(is.na(agyeartreat1)) != nrow(agyeartreat1)]#remove columns that are all NA
agyeartreat2<-agyearmns[,,3]
agyeartreat2<-agyeartreat2[, colSums(is.na(agyeartreat2)) != nrow(agyeartreat2)]#remove columns that are all NA
agyeartreat3<-agyearmns[,,4]
agyeartreat3<-agyeartreat3[, colSums(is.na(agyeartreat3)) != nrow(agyeartreat3)]#remove columns that are all NA
#Difference between treatments and mean control
agyearwarm1<-agyeartreat1-agyearcontrols
agyearwarm2<-agyeartreat2-agyearcontrols[,2:5]
agyearwarm3<-agyeartreat3-agyearcontrols[,2:5]

#target warming
agtarget1<-target1[-which(rownames(target1)=="farnsworth"),]
#plot difference between warmed and control, by block and year
quartz(height=7,width=10)
par(mfrow=c(1,2))
plot(c(agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1),c(agwarm1[,1],agwarm1[,2],agwarm1[,3],agwarm1[,4]),pch=rep(c(21,24), times=6),col="black",xlab="Target warming (C)", ylab="Observed above-ground warming (C)", bty="l", main="By Block", xlim=c(0,6), ylim=c(0,6))
points(c(agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2),c(agwarm2[,1],agwarm2[,2],agwarm2[,3],agwarm2[,4]))
points(c(agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3),c(agwarm3[,1],agwarm3[,2],agwarm3[,3],agwarm3[,4]))
legend(1,6,pch=c(21,24),legend=rownames(agtarget1),bty="n")
plot(c(agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1),c(agyearwarm1[,1],agyearwarm1[,2],agyearwarm1[,3],agyearwarm1[,4],agyearwarm1[,5]),pch=rep(c(21,24), times=5),col="black",xlab="Target warming (C)", ylab="Observed above-ground warming (C)", bty="l", main="By Year",xlim=c(0,6), ylim=c(0,6))
points(c(agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2),c(agyearwarm2[,1],agyearwarm2[,2],agyearwarm2[,3],agyearwarm2[,4]),pch=rep(c(21,24),times=4))
points(c(agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3),c(agyearwarm3[,1],agyearwarm3[,2],agyearwarm3[,3],agyearwarm3[,4]),pch=rep(c(21,24),times=4))



#Statistical tests to test if there are differences in temperature treatment by block and year the below is not done- need to think about this more....
blockdat2<-subset(blockdat,select=c(site,block,year,temptreat,soiltemp1_mean,temptreat))
blockdat2$block<-as.factor(block2dat$block)
block2dat$year<-as.factor(block2dat$year)
blockdat2  <- blockdat2 [apply(blockdat2 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
blockmod<-lmer(soiltemp1_mean~temptreat*block + (1|site/year), data= blockdat, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
summary(blockmod)
Anova(blockmod, type="III")
yearmod<-lmer(soiltemp1_mean~temptreat*year + (1|site/block), data= blockdat2, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
Anova(yearmod,type="III")
