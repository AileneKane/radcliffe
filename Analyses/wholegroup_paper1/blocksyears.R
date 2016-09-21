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
#blockdat<-blockdat[which(blockdat$preciptreat==0|is.na(blockdat$preciptreat)),]#remove precip treatments
blockdat$treat<-paste(blockdat$temptreat,blockdat$preciptreat, sep=".")
blockdat[which(blockdat$treat=="0.NA" |blockdat$treat=="0.0"),]$treat<-"control"
blockdat[which(blockdat$treat=="1.NA" |blockdat$treat=="1.0"),]$treat<-"temp1"
blockdat[which(blockdat$treat=="2.0"),]$treat<-"temp2"
blockdat[which(blockdat$treat=="3.0"),]$treat<-"temp3"
blockdat[which(blockdat$treat=="0.1"),]$treat<-"controlprecip1"
blockdat[which(blockdat$treat=="0.-1"),]$treat<-"controlprecip-1"
blockdat[which(blockdat$treat=="1.1"),]$treat<-"temp1precip1"
blockdat[which(blockdat$treat=="2.1"),]$treat<-"temp2precip1"
blockdat[which(blockdat$treat=="3.1"),]$treat<-"temp3precip1"
blockdat[which(blockdat$treat=="1.-1"),]$treat<-"temp1precip-1"
blockdat[which(blockdat$treat=="2.-1"),]$treat<-"temp2precip-1"
blockdat[which(blockdat$treat=="3.-1"),]$treat<-"temp3precip-1"
blockmns<-tapply(blockdat$soiltemp1_mean,list(blockdat$site,blockdat$block,blockdat$treat), mean, na.rm=TRUE)
blockmns<-blockmns[which(rownames(blockmns)=="exp01"|rownames(blockmns)=="exp08"|rownames(blockmns)=="exp09"),,]
blockdat$site<-factor(blockdat$site)
blockdat$block<-factor(blockdat$block)
controls<-blockmns[,1:6,5]
treat1<-blockmns[,1:6,8]
treat2<-blockmns[,1:6,11]
treat3<-blockmns[,1:6,14]
wet0<-blockmns[,1:6,7]
dry0<-blockmns[,1:6,6]
wet1<-blockmns[,1:6,10]
dry1<-blockmns[,1:6,9]
wet2<-blockmns[,1:6,13]
dry2<-blockmns[,1:6,12]
wet3<-blockmns[,1:6,16]
dry3<-blockmns[,1:6,15]
#Difference between temptreatments and mean control
warm1<-treat1-controls
warm2<-treat2-controls
warm3<-treat3-controls

#Difference between treatments and mean control
wet0dif<-wet0-controls
wet1dif<-wet1-controls
wet2dif<-wet2-controls
wet3dif<-wet3-controls
dry0dif<-dry0-controls
dry1dif<-dry1-controls
dry2dif<-dry2-controls
dry3dif<-dry3-controls

#Now by year instead of by block
yearmns<-tapply(blockdat$soiltemp1_mean,list(blockdat$site,blockdat$year,blockdat$treat), mean, na.rm=TRUE)
yearmns<-yearmns[which(rownames(yearmns)=="exp01"|rownames(yearmns)=="exp08"|rownames(yearmns)=="exp09"),,]
yearcontrols<-yearmns[,,5]
yearcontrols<-yearcontrols[,colSums(is.na(yearcontrols)) != nrow(yearcontrols)]#remove columns that are all NA
yeartreat1<-yearmns[,,8]
yeartreat1<-yeartreat1[,colSums(is.na(yeartreat1)) != nrow(yeartreat1)]#remove columns that are all NA
yeartreat2<-yearmns[,,11]
yeartreat2<-yeartreat2[,colSums(is.na(yeartreat2)) != nrow(yeartreat2)]#remove columns that are all NA
yeartreat3<-yearmns[,,14]
yeartreat3<-yeartreat3[,colSums(is.na(yeartreat3)) != nrow(yeartreat3)]#remove columns that are all NA
yearwet0<-yearmns[,,7]
yeardry0<-yearmns[,,6]
yeardry0<-yeardry0[, colSums(is.na(yeardry0)) != nrow(yeardry0)]#remove columns that are all NA
yearwet0<-yearwet0[, colSums(is.na(yearwet0)) != nrow(yearwet0)]#remove columns that are all NA
yearwet1<-yearmns[,,10]
yeardry1<-yearmns[,,9]
yeardry1<-yeardry1[,colSums(is.na(yeardry1)) != nrow(yeardry1)]#remove columns that are all NA
yearwet1<-yearwet1[,colSums(is.na(yearwet1)) != nrow(yearwet1)]#remove columns that are all NA
yearwet2<-yearmns[,,13]
yeardry2<-yearmns[,,12]
yeardry2<-yeardry2[,colSums(is.na(yeardry2)) != nrow(yeardry2)]#remove columns that are all NA
yearwet2<-yearwet2[,colSums(is.na(yearwet2)) != nrow(yearwet2)]#remove columns that are all NA
yearwet3<-yearmns[,,16]
yeardry3<-yearmns[,,15]
yeardry3<-yeardry3[,colSums(is.na(yeardry3)) != nrow(yeardry3)]#remove columns that are all NA
yearwet3<-yearwet3[,colSums(is.na(yearwet3)) != nrow(yearwet3)]#remove columns that are all NA

#Difference between treatments and mean control
yearwarm1<-yeartreat1-yearcontrols
yearwarm2<-yeartreat2-yearcontrols[,3:6]
yearwarm3<-yeartreat3-yearcontrols[,3:6]
yearwet0dif<-yearwet0-yearcontrols[,2:6]
yearwet1dif<-yearwet1-yearcontrols[,2:6]
yearwet2dif<-yearwet2-yearcontrols[,3:6]
yearwet3dif<-yearwet3-yearcontrols[,3:6]
yeardry0dif<-yeardry0-yearcontrols[,3:6]
yeardry1dif<-yeardry1-yearcontrols[,3:6]
yeardry2dif<-yeardry2-yearcontrols[,3:6]
yeardry3dif<-yeardry3-yearcontrols[,3:6]

#target warming
treats<-read.csv("expsiteinfo.csv", header=T)
target1<-treats[treats$DatasetID=="exp01"|treats$DatasetID=="exp08"|treats$DatasetID=="exp09",27:29]
rownames(target1)<-treats[as.numeric(rownames(target1)),]$DatasetID

#plot difference between warmed and control, by block and year
quartz(height=6,width=11)
par(mfrow=c(1,2))
plot(c(rep(target1$temptreat_1,times=6)),c(warm1[,1],warm1[,2],warm1[,3],warm1[,4],warm1[,5],warm1[,6]),pch=rep(c(21,22,24), times=6),col="black",bg=c("black","black","black"),xlab="Target warming (C)", ylab="Observed soil warming (C)", bty="l", main="By Block", xlim=c(0,6), ylim=c(0,6))
points(c(rep(target1$temptreat_2,times=6)),c(warm2[,1],warm2[,2],warm2[,3],warm2[,4],warm2[,5],warm2[,6]),pch=21,col="black",bg=c("black","black","black"))
points(c(rep(target1$temptreat_3,times=6)),c(warm3[,1],warm3[,2],warm3[,3],warm3[,4],warm3[,5],warm3[,6]),pch=21,col="black",bg=c("black","black","black"))
abline(a=0,b=1,lty=1)
#legend(0,6,pch=c(21,22,24),pt.bg="black",legend=rownames(target1),bty="n")
#add precip treatment points
points(c(rep(0,times=12)),c(wet0dif[,1],wet0dif[,2],wet0dif[,3],wet0dif[,4]),pch=rep(c(21,22,24), times=4),col="black",bg=c("blue","blue","blue"))
points(c(rep(target1$temptreat_1,times=4)),c(wet1dif[,1],wet1dif[,2],wet1dif[,3],wet1dif[,4]),pch=rep(c(21,22,24), times=4),col="black",bg=c("blue","blue","blue"))
points(c(rep(target1$temptreat_2,times=4)),c(wet2dif[,1],wet2dif[,2],wet2dif[,3],wet2dif[,4]),pch=rep(c(21,22,24), times=4),col="black",bg=c("blue","blue","blue"))
points(c(rep(target1$temptreat_3,times=4)),c(wet3dif[,1],wet3dif[,2],wet3dif[,3],wet3dif[,4]),pch=rep(c(21,22,24), times=4),col="black",bg=c("blue","blue","blue"))
points(c(rep(0,times=12)),c(dry0dif[,1],dry0dif[,2],dry0dif[,3],dry0dif[,4]),pch=rep(c(21,22,24), times=4),col="black",bg=c("white","white","white"))
points(c(rep(target1$temptreat_1,times=4)),c(dry1dif[,1],dry1dif[,2],dry1dif[,3],dry1dif[,4]),pch=rep(c(21,22,24), times=4),col="black",bg=c("white","white","white"))
points(c(rep(target1$temptreat_2,times=4)),c(dry2dif[,1],dry2dif[,2],dry2dif[,3],dry2dif[,4]),pch=rep(c(21,22,24), times=4),col="black",bg=c("white","white","white"))
points(c(rep(target1$temptreat_3,times=4)),c(dry3dif[,1],dry3dif[,2],dry3dif[,3],dry3dif[,4]),pch=rep(c(21,22,24), times=4),col="black",bg=c("white","white","white"))

plot(c(target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1),c(yearwarm1[,1],yearwarm1[,2],yearwarm1[,3],yearwarm1[,4],yearwarm1[,5]),pch=rep(c(21,22,24), times=6),bg="black",xlab="Target warming (C)", ylab="Observed soil warming (C)", bty="l", main="By Year",xlim=c(0,6), ylim=c(0,6))
points(c(target1$temptreat_2,target1$temptreat_2,target1$temptreat_2,target1$temptreat_2),c(yearwarm2[,1],yearwarm2[,2],yearwarm2[,3],yearwarm2[,4]),pch=rep(c(21,22,24), times=4), bg="black")
points(c(target1$temptreat_3,target1$temptreat_3,target1$temptreat_3,target1$temptreat_3),c(yearwarm3[,1],yearwarm3[,2],yearwarm3[,3],yearwarm3[,4]),pch=rep(c(21,22,24), times=4),bg="black")
abline(a=0,b=1,lty=1)
#add precip treatment points
points(c(rep(0,times=15)),c(yearwet0dif[,1],yearwet0dif[,2],yearwet0dif[,3],yearwet0dif[,4],yearwet0dif[,5]),pch=rep(c(21,22,24), times=5),col="black",bg=c("blue","blue","blue"))
points(c(rep(target1$temptreat_1,times=5)),c(yearwet1dif[,1],yearwet1dif[,2],yearwet1dif[,3],yearwet1dif[,4],yearwet1dif[,5]),pch=rep(c(21,22,24), times=5),col="black",bg=c("blue","blue","blue"))
points(c(rep(target1$temptreat_2,times=4)),c(yearwet2dif[,1],yearwet2dif[,2],yearwet2dif[,3],yearwet2dif[,4]),pch=rep(c(21,22,24), times=5),col="black",bg=c("blue","blue","blue"))
points(c(rep(target1$temptreat_3,times=4)),c(yearwet3dif[,1],yearwet3dif[,2],yearwet3dif[,3],yearwet3dif[,4]),pch=rep(c(21,22,24), times=5),col="black",bg=c("blue","blue","blue"))
points(c(rep(0,times=12)),c(yeardry0dif[,1],yeardry0dif[,2],yeardry0dif[,3],yeardry0dif[,4]),pch=rep(c(21,22,24), times=5),col="black",bg=c("white","white","white"))
points(c(rep(target1$temptreat_1,times=4)),c(yeardry1dif[,1],yeardry1dif[,2],yeardry1dif[,3],yeardry1dif[,4]),pch=rep(c(21,22,24), times=4),col="black",bg=c("white","white","white"))
points(c(rep(target1$temptreat_2,times=4)),c(yeardry2dif[,1],yeardry2dif[,2],yeardry2dif[,3],yeardry2dif[,4]),pch=rep(c(21,22,24), times=4),col="black",bg=c("white","white","white"))
points(c(rep(target1$temptreat_3,times=4)),c(yeardry3dif[,1],yeardry3dif[,2],yeardry3dif[,3],yeardry3dif[,4]),pch=rep(c(21,22,24), times=4),col="black",bg=c("white","white","white"))

#Try for aboveground temp
blockdat$agtemp_mean<-NA
blockdat$agtemp_mean<-(blockdat$airtemp_min+blockdat$airtemp_max)/2
blockdat[which(!is.na(blockdat$cantemp_min)),]$agtemp_mean<-(blockdat[which(!is.na(blockdat$cantemp_min)),]$cantemp_max+blockdat[which(!is.na(blockdat$cantemp_min)),]$cantemp_min)/2
blockdat[which(!is.na(blockdat$surftemp_min)),]$agtemp_mean<-(blockdat[which(!is.na(blockdat$surftemp_min)),]$surftemp_max+blockdat[which(!is.na(blockdat$surftemp_min)),]$surftemp_min)/2
agblockmns<-tapply(blockdat$agtemp_mean,list(blockdat$site,blockdat$block,blockdat$temptreat), mean, na.rm=TRUE)
agblockmns<-agblockmns[which(rownames(agblockmns)=="exp01"|rownames(agblockmns)=="exp09"),,]
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
agyearmns<-agyearmns[which(rownames(agyearmns)=="exp01"|rownames(agyearmns)=="exp09"),,]
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
agtarget1<-target1[-which(rownames(target1)=="exp08"),]
quartz(height=6,width=11)
par(mfrow=c(1,2))
plot(c(agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1),c(agwarm1[,1],agwarm1[,2],agwarm1[,3],agwarm1[,4]),pch=rep(c(21,24), times=6),col="black",bg=c("black","black"),xlab="Target warming (C)", ylab="Observed above-ground warming (C)", bty="l", xlim=c(0,6), ylim=c(0,6))
points(c(agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2),c(agwarm2[,1],agwarm2[,2],agwarm2[,3],agwarm2[,4]),pch=rep(c(21,24), times=6),col="black",bg=c("black","black"))
points(c(agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3),c(agwarm3[,1],agwarm3[,2],agwarm3[,3],agwarm3[,4]),pch=rep(c(21,24), times=6),col="black",bg="black")
abline(a=0,b=1,lty=1)
legend(1,6,pch=c(21,24),pt.bg="black",legend=rownames(agtarget1),bty="n")
plot(c(agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1),c(agyearwarm1[,1],agyearwarm1[,2],agyearwarm1[,3],agyearwarm1[,4],agyearwarm1[,5]),pch=rep(c(21,24), times=5),col="black",bg="black", ylab="Observed above-ground warming (C)", bty="l", xlab="Target warming (C)",xlim=c(0,6), ylim=c(0,6))
points(c(agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2),c(agyearwarm2[,1],agyearwarm2[,2],agyearwarm2[,3],agyearwarm2[,4]),pch=rep(c(21,24),times=4),col="black",bg="black")
points(c(agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3),c(agyearwarm3[,1],agyearwarm3[,2],agyearwarm3[,3],agyearwarm3[,4]),pch=rep(c(21,24),times=4),col="black",bg="black")
abline(a=0,b=1,lty=1)
###Try making same figure with above-ground and soil temperature shown
#plot difference between warmed and control, by block and year
quartz(height=6.5,width=6.5)
par(mfrow=c(2,2),mai=c(.5,.7,.2,.01),omi=c(.7,.3,.2,.7))
plot(c(agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1),c(agwarm1[,1],agwarm1[,2],agwarm1[,3],agwarm1[,4]),pch=rep(c(21,24), times=6),col="black",bg=c("black","black"),xlab="",ylab="Above-ground", bty="l", main="By Block", xlim=c(0,6), ylim=c(0,6), cex.axis=1.2,cex.lab=1.3)
points(c(agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2),c(agwarm2[,1],agwarm2[,2],agwarm2[,3],agwarm2[,4]),pch=rep(c(21,24), times=6),col="black",bg=c("black","black"))
points(c(agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3),c(agwarm3[,1],agwarm3[,2],agwarm3[,3],agwarm3[,4]),pch=rep(c(21,24), times=6),col="black",bg="black")
abline(a=0,b=1,lty=1)
plot(c(agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1),c(agyearwarm1[,1],agyearwarm1[,2],agyearwarm1[,3],agyearwarm1[,4],agyearwarm1[,5]),pch=rep(c(21,24), times=5),col="black",bg="black",xlab="",ylab="", bty="l", main="By Year",xlim=c(0,6), ylim=c(0,6), cex.axis=1.2,cex.lab=1.2)
points(c(agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2),c(agyearwarm2[,1],agyearwarm2[,2],agyearwarm2[,3],agyearwarm2[,4]),pch=rep(c(21,24),times=4),col="black",bg="black")
points(c(agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3),c(agyearwarm3[,1],agyearwarm3[,2],agyearwarm3[,3],agyearwarm3[,4]),pch=rep(c(21,24),times=4),col="black",bg="black")
abline(a=0,b=1,lty=1)
plot(c(rep(target1$temptreat_1,times=6)),c(warm1[,1],warm1[,2],warm1[,3],warm1[,4],warm1[,5],warm1[,6]),pch=rep(c(21,22,24), times=6),col="black",bg=c("black","black","black"),xlab="Target warming (C)", ylab="Soil", bty="l", xlim=c(0,6), ylim=c(0,6), cex.axis=1.2,cex.lab=1.2)
points(c(rep(target1$temptreat_2,times=6)),c(warm2[,1],warm2[,2],warm2[,3],warm2[,4],warm2[,5],warm2[,6]),pch=21,col="black",bg=c("black","black","black"))
points(c(rep(target1$temptreat_3,times=6)),c(warm3[,1],warm3[,2],warm3[,3],warm3[,4],warm3[,5],warm3[,6]),pch=21,col="black",bg=c("black","black","black"))
abline(a=0,b=1,lty=1)
mtext("Observed warming (C)", side=2,line=4.5,adj=14, cex=1.2)

plot(c(target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1),c(yearwarm1[,1],yearwarm1[,2],yearwarm1[,3],yearwarm1[,4],yearwarm1[,5]),pch=rep(c(21,22,24), times=6),bg="black",xlab="Target warming (C)", ylab="", bty="l", xlim=c(0,6), ylim=c(0,6), cex.axis=1.2,cex.lab=1.2)
points(c(target1$temptreat_2,target1$temptreat_2,target1$temptreat_2,target1$temptreat_2),c(yearwarm2[,1],yearwarm2[,2],yearwarm2[,3],yearwarm2[,4]),pch=rep(c(21,22,24), times=4), bg="black")
points(c(target1$temptreat_3,target1$temptreat_3,target1$temptreat_3,target1$temptreat_3),c(yearwarm3[,1],yearwarm3[,2],yearwarm3[,3],yearwarm3[,4]),pch=rep(c(21,22,24), times=4),bg="black")
abline(a=0,b=1,lty=1)
legend(5,2,pch=c(21,22,24),pt.bg="black",legend=c("exp01","exp08","exp09"),bty="n")
mtext("Target warming (C)", side=1,line=3,adj=-2.3, cex=1.2)

#Statistical tests to test if there are differences in temperature treatment by block and year the below is not done- need to think about this more....
blockdat2<-subset(blockdat,select=c(site,block,year,temptreat,soiltemp1_mean,temptreat))
blockdat2$block<-as.factor(blockdat2$block)
blockdat2$year<-as.factor(blockdat2$year)
blockdat2  <- blockdat2 [apply(blockdat2 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
blockdat2<-blockdat2[-which(blockdat2$temptreat=="ambient"),]
blockmod<-lmer(soiltemp1_mean~temptreat*block + (1|site/year), data= blockdat2, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
summary(blockmod)
Anova(blockmod, type="III")
yearmod<-lmer(soiltemp1_mean~temptreat*year + (1|site/block), data= blockdat2, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
Anova(yearmod,type="III")

#test, to see what may be causing rank deficiency in model:
testdat<-blockdat2[as.numeric(as.character(blockdat2$block))<4,]
blockmod<-lmer(soiltemp1_mean~temptreat*block + (1|site/year), data= testdat, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
Anova(blockmod, type="III")
summary(blockmod)
#tried runnning with only blocks 1-3 (to remove rank deficiency error. in this case, temptreat is still sig. and interaction is still dignificant, but block itself is not significant- still supports point!)
#no warning
testdat2<-blockdat2[which(as.numeric(as.character(blockdat2$year))<2011),]
testdat2<-testdat2[which(as.numeric(as.character(testdat2$year))>2008),]
testdat2$year<-as.factor(testdat2$year)
yearmod<-lmer(soiltemp1_mean~temptreat*year + (1|site/block), data= testdat2, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
summary(yearmod)
Anova(yearmod, type="III")
#no warning
