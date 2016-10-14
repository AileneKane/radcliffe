#Additional analyses for Experimental Climate Paper
#Analysis of Mean, Min & Max Temperature (and variance) compared to target and reported
#a.	Do treatments match their target/reported? What is the mean difference? 

#b.	Are variances similar across controls and treatments?
#c.	Yann’s analysis of difference between min and max
#d.	“I suggest that warming treatments also buffer extreme cold events. It would be nice to compare for instance minimum temperatures (or absolute minimum temperature) in spring between the control and the warming treatment and to see if this difference is bigger than between the mean or max temperature. I expect it to be much larger during cold night with clear sky due to radiative cooling in the control that is not occuring in the warming plot due to artificial warming...”
setwd("~/git/radcliffe/Analyses")
rm(list=ls()) 
options(stringsAsFactors=FALSE)

library(lme4)
library(car)
expclim<-read.csv("expclim.csv", header=T)
treats<-read.csv("treats_detail.csv", header=T)
effwarm.plot <- read.csv("EffectiveWarming_Plot.csv", header=TRUE)#i think the treats file and effective warming should have the same number of rows- one per site-plot, right? why aren't they matching up?
exp.tarrep <- join(treats,effwarm.plot, by=c("site", "block", "plot","temptreat","preciptreat"), match="first")
###First just plot Target vs. reported and Reported vs. our measure
#select out only plots with temperature manipulation
tempexp<-exp.tarrep[which(exp.tarrep$preciptreat==0|is.na(exp.tarrep$preciptreat)),]
tempexp2<-tempexp[which(as.numeric(tempexp$temptreat)>= 0),]#remove ambient controls?
treatcol<-c("gray","#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
quartz(height=6.5,width=6.5)
par(mfrow=c(1,2),mai=c(.5,.7,.2,.01),omi=c(.7,.3,.2,.7))
plot(as.numeric(tempexp2$target),as.numeric(tempexp2$reported),pch=as.numeric(as.factor(tempexp2$site)),col=treatcol[as.numeric(tempexp2$temptreat)],xlab="Target temp",ylab="Reported Temp", bty="l")
abline(a=0,b=1,lty=1)
plot(as.numeric(tempexp2$target),as.numeric(tempexp2$AGtemp_mean_dev),pch=as.numeric(as.factor(tempexp2$site)),col=treatcol[as.numeric(tempexp2$temptreat)],xlab="Target temp",ylab="Above-ground Temp", bty="l")
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


###
dim(exp.tarrep)
blockdat<-expclim[which(!is.na(expclim$block)),]