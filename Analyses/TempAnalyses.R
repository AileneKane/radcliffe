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
sitesymb<-c(21,22,16,24,25,17,3,4,5,10,15,12)
cols_t1<-treatcol[tempexp2$target]
symb_site<-sitesymb[factor(tempexp2$site)]
#plot
quartz(height=5,width=9)
par(mfrow=c(1,2),mai=c(.9,.9,.2,.01),omi=c(.3,.3,.2,.3))
plot(as.numeric(tempexp2$target),as.numeric(tempexp2$reported),pch=sitesymb[as.numeric(as.factor(tempexp2$site))],col=treatcol[as.numeric(tempexp2$target)],xlab="Target temp",ylab="Reported Temp", bty="l", cex.axis=.9)
abline(a=0,b=1,lty=1)
plot(as.numeric(tempexp2$target),as.numeric(tempexp2$AGtemp_mean_dev),pch=sitesymb[tempexp2$site],col=treatcol[as.numeric(tempexp2$target)],xlab="Target temp",ylab="AGTemp Mean Dev", bty="l",, cex.axis=.9)
abline(a=0,b=1,lty=1)
par(mfrow=c(2,2),mai=c(.9,.9,.2,.01),omi=c(.3,.3,.2,.3))

quartz(height=7,width=9)
par(mfrow=c(2,2),mai=c(.9,.9,.2,.01),omi=c(.3,.3,.2,.3))
plot(as.numeric(tempexp2$target),as.numeric(tempexp2$reported),pch=symb_site,col="black",xlab="Target temp difference (C)",ylab="Reported temp difference (C)", bty="l", cex.axis=.9, ylim=c(0,5),xlim=c(0,5))
abline(a=0,b=1,lty=1)
legend("topleft",legend=unique(tempexp2$site),pch=unique(symb_site),bty="n",cex=0.7,pt.cex=0.7)
plot(as.numeric(tempexp2$target),as.numeric(tempexp2$AGtemp_mean_dev),pch=symb_site,col="black",xlab="Target temp difference (C)",ylab="AGTemp Mean Dev", bty="l",, cex.axis=.9,ylim=c(0,5),xlim=c(0,5))
abline(a=0,b=1,lty=1)
plot(as.numeric(tempexp2$target),as.numeric(tempexp2$AGtemp_max_dev),pch=symb_site,col="black",xlab="Target temp difference (C)",ylab="AGTemp Max Dev (C)", bty="l", cex.axis=.9, ylim=c(0,5),xlim=c(0,5))
abline(a=0,b=1,lty=1)
plot(as.numeric(tempexp2$target),as.numeric(tempexp2$AGtemp_min_dev ),pch=symb_site,col="black",xlab="Target temp difference (C)",ylab="AGTemp Min Dev (C)", bty="l",, cex.axis=.9,ylim=c(0,5),xlim=c(0,5))
abline(a=0,b=1,lty=1)
#now look at min  and max variance
var()
#Now look at min and max temp
head(tempexp2)
head(expclim)
expclim3<-subset(expclim,select=c("site","temptreat","airtemp_min","airtemp_max","cantemp_min", "cantemp_max","surftemp_min","surftemp_max"))
#want to compare mean and variances of min and max temperatures in control plots and warmed plots in each study
#using two types structural controls separately
expclimt<-expclim[which(expclim$preciptreat==0|is.na(expclim$preciptreat)),]
expclimt$agtemp_min<-expclimt$airtemp_min
expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$cantemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$airtemp_min) & !is.na(expclimt$cantemp_min)),]$cantemp_min
expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$surftemp_min
expclimt$agtemp_max<-expclimt$airtemp_max
expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$cantemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$airtemp_max) & !is.na(expclimt$cantemp_max)),]$cantemp_max
expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$surftemp_max

sites<-sort(unique(expclimt$site))#use data without precipitation manipulation
temptreats<-sort(unique(expclimt$temptreat))
#alltemp.df <- data.frame(site=character(),tempmax.mn.0=numeric(),
#    tempmax.mn.1=numeric(),tempmax.mn.2=numeric(),tempmax.mn.3=numeric(),
 #   tempmax.mn.4=numeric(),tempmax.mn.5=numeric(),tempmax.mn.6=numeric(),
  #  tempmax.mn.7=numeric(),tempmax.mn.8=numeric(),tempmax.mn.9=numeric(),
   # tempmax.mn.amb=numeric(),tempmax.var.0=numeric(),
  #  tempmax.var.1=numeric(),tempmax.var.2=numeric(),tempmax.var.3=numeric(),
  #  tempmax.var.4=numeric(),tempmax.var.5=numeric(),tempmax.var.6=numeric(),
  #  tempmax.var.7=numeric(),tempmax.var.8=numeric(),tempmax.var.9=numeric(),
  #  tempmax.var.amb=numeric(),tempmin.mn.0=numeric(),
  #  tempmin.mn.1=numeric(),tempmin.mn.2=numeric(),tempmin.mn.3=numeric(),
  #  tempmin.mn.4=numeric(),tempmin.mn.5=numeric(),tempmin.mn.6=numeric(),
  #  tempmin.mn.7=numeric(),tempmin.mn.8=numeric(),tempmin.mn.9=numeric(),
  #  tempmin.mn.amb=numeric(),tempmin.var.0=numeric(),
  #  tempmin.var.1=numeric(),tempmin.var.2=numeric(),tempmin.var.3=numeric(),
  #  tempmin.var.4=numeric(),tempmin.var.5=numeric(),tempmin.var.6=numeric(),
  #  tempmin.var.7=numeric(),tempmin.var.8=numeric(),tempmin.var.9=numeric(),
  #  tempmin.var.amb=numeric(),stringsAsFactors=FALSE) 
quartz()
for (i in 1:length(sites)){
  sitedat<-expclimt[expclimt$site==sites[i],]
  siteagtempmaxmn<-tapply(sitedat$agtemp_max,sitedat$temptreat,mean,na.rm=TRUE)
  siteagtempmaxvar<-tapply(sitedat$agtemp_max,sitedat$temptreat,var,na.rm=TRUE)
  siteagtempminmn<-tapply(sitedat$agtemp_min,sitedat$temptreat,mean,na.rm=TRUE)
  siteagtempminvar<-tapply(sitedat$agtemp_min,sitedat$temptreat,var,na.rm=TRUE)
  sitebgtempmaxmn<-tapply(sitedat$soiltemp1_max,sitedat$temptreat,mean,na.rm=TRUE)
  sitebgtempmaxvar<-tapply(sitedat$soiltemp1_max,sitedat$temptreat,var,na.rm=TRUE)
  sitebgtempminmn<-tapply(sitedat$soiltemp1_min,sitedat$temptreat,mean,na.rm=TRUE)
  sitebgtempminvar<-tapply(sitedat$soiltemp1_min,sitedat$temptreat,var,na.rm=TRUE)
  sitetreats<-length(unique(sitedat$temptreat))
  sitename<-rep(paste(sites[i]),times=sitetreats)
  #empties<-rep(NA, times=length(temptreats)-sitetreats)
  sitetemp<-cbind(sitename,names(siteagtempmaxmn),round(siteagtempmaxmn, digits=3),round(siteagtempmaxvar,digits=3),round(siteagtempminmn,digits=3),round(siteagtempminvar,digits=3),round(sitebgtempmaxmn, digits=3),round(sitebgtempmaxvar,digits=3),round(sitebgtempminmn,digits=3),round(sitebgtempminvar,digits=3))
  alltemp.df<-rbind(alltemp.df,sitetemp)
}
colnames(alltemp.df)<-c("site","temptreat","AGTempMax_Mn","AGTempMax_Var","AGTempMin_Mn","AGTempMin_Var","BGTempMax_Mn","BGTempMax_Var","BGTempMin_Mn","BGTempMin_Var")
alltemp.df$AGTempMax_Mn<-as.numeric(alltemp.df$AGTempMax_Mn)
alltemp.df$AGTempMax_Var<-as.numeric(alltemp.df$AGTempMax_Var)
alltemp.df$AGTempMin_Mn<-as.numeric(alltemp.df$AGTempMin_Mn)
alltemp.df$AGTempMin_Var<-as.numeric(alltemp.df$AGTempMin_Var)
alltemp.df$BGTempMax_Mn<-as.numeric(alltemp.df$BGTempMax_Mn)
alltemp.df$BGTempMax_Var<-as.numeric(alltemp.df$BGTempMax_Var)
alltemp.df$BGTempMin_Mn<-as.numeric(alltemp.df$BGTempMin_Mn)
alltemp.df$BGTempMin_Var<-as.numeric(alltemp.df$BGTempMin_Var)

treats2<-subset(treats,select=c("site","temptreat","target","reported"))
treats2 <- treats %>% # start with the data frame
  distinct(site, temptreat,.keep_all = TRUE) %>% # establishing grouping variables
  select(site,temptreat,target,reported)

alltemptarget <- join(treats2,alltemp.df, by=c("site","temptreat"), match="first")
head(alltemptarget)
#for control plots with structures, use target warming=0
alltemptarget[which(alltemptarget$temptreat==0),]$target<-0
#for ambient control plots, use target warming = -1
alltemptarget[which(alltemptarget$temptreat=="ambient"),]$target<--1

#plot variance by target warming
#remove exp02 (chuine), as these variances aren't real
alltemptarget<-alltemptarget[-which(alltemptarget$site=="exp02"),]
quartz(height=5,width=10)
par(mfrow=c(2,2),mai=c(.3,.6,.2,.05),omi=c(.5,.5,.2,.5))
plot(alltemptarget$target,alltemptarget$AGTempMax_Var,pch=sitesymb[as.numeric(as.factor(alltemptarget$site))],xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(20,170), bty="l", cex.axis=.9, main="Max AG temp")
#mtext(side=2,"Max AG temp", line=2.2, adj=.8, cex=.9)
axis(side=2,at=c(50,100,150), labels=TRUE, las=TRUE, cex=.9)
mtext(side=2,"Variance in temperature (C)", line=4,adj=3,cex=.9)
plot(alltemptarget$target,alltemptarget$AGTempMin_Var,pch=sitesymb[as.numeric(as.factor(alltemptarget$site))],xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(20,170), bty="l",, cex.axis=.9, main="Min AG temp")
axis(side=2,at=c(50,100,150), labels=TRUE, las=TRUE, cex=.9)

#mtext(side=2,"Min AG temp", line=1, adj=.8, cex=.9)
plot(alltemptarget$target,alltemptarget$BGTempMax_Var,pch=sitesymb[as.numeric(as.factor(alltemptarget$site))],xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(20,170), bty="l", cex.axis=.9,main="Max BG temp")
axis(side=2,at=c(50,100,150), labels=TRUE, las=TRUE, cex=.9)
axis(side=1,at=c(-1,0,1,2,3,4,5), labels=c("ambient","0","1","2","3","4","5"),cex=.9)
mtext(side=1,"Target warming (C)", line=2.3, adj=1.5)
plot(alltemptarget$target,alltemptarget$BGTempMin_Var,pch=sitesymb[as.numeric(as.factor(alltemptarget$site))],xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(20,170), bty="l",, cex.axis=.9, main="Min BG temp")
axis(side=1,at=c(-1,0,1,2,3,4,5), labels=c("ambient","0","1","2","3","4","5"),cex=.9)
axis(side=2,at=c(50,100,150), labels=TRUE, las=TRUE, cex=.9)
mtext(side=1,"Target warming (C)", line=2.3, adj=1.5)
