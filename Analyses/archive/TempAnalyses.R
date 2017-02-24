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
library(raster)
library(RColorBrewer)
library(dplyr)
library(tidyr)

expclim<-read.csv("expclim.csv", header=T)
treats<-read.csv("treats_detail.csv", header=T)
effwarm.plot <- read.csv("EffectiveWarming_Plot.csv", header=TRUE)#i think the treats file and effective warming should have the same number of rows- one per site-plot, right? why aren't they matching up?
exp.tarrep <- full_join(treats,effwarm.plot, by=c("site", "block", "plot","temptreat","preciptreat"), match="first")

#this join is not quite right- many extra rows with no information?
###First just plot Target vs. reported and Reported vs. our measure
#select out only plots with temperature manipulation
tempexp<-exp.tarrep[which(exp.tarrep$preciptreat==0|is.na(exp.tarrep$preciptreat)),]
tempexp2<-tempexp[which(as.numeric(tempexp$temptreat)>= 0),]#remove ambient controls?
treatcol<-c("gray","#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
sitesymb<-c(21,22,16,24,25,17,3,4,5,10,15,12)
cols_t1<-treatcol[tempexp2$target]
symb_site<-sitesymb[factor(tempexp2$site)]
#plot
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

expclim3<-subset(expclim,select=c("site","temptreat","airtemp_min","airtemp_max","cantemp_min", "cantemp_max","surftemp_min","surftemp_max"))
#want to compare mean, dtr, and variances of min and max temperatures in control plots and warmed plots in each study
#using two types structural controls separately
expclim2<-full_join(treats,expclim, by=c("site", "block", "plot","temptreat","preciptreat"), match="first")

sites<-sort(unique(expclim2$site))#use data without precipitation manipulation
temptreats<-sort(unique(expclim2$temptreat))
alltemp.df <- data.frame(site=character(),temptreat=character(),agtempmax_mn=numeric(),
                         agtempmax_var=numeric(),agtempmin_mn=numeric(),
                         agtempmin_var=numeric(),agtempmax_mn=numeric(),
                         agtempmax_var=numeric(),agtempmin_mn=numeric(),
                         agtempmin_var=numeric())

for (i in 1:length(sites)){
  sitedat<-expclim2[expclim2$site==sites[i],]
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
  dplyr::select(site,temptreat,target,reported)

alltemptarget <- full_join(treats2,alltemp.df, by=c("site","temptreat"), match="first")
head(alltemptarget)
#for control plots with structures, use target warming=0
alltemptarget[which(alltemptarget$temptreat==0),]$target<-0
#for ambient control plots, use target warming = -1
alltemptarget[which(alltemptarget$temptreat=="ambient"),]$target<--1

#plot variance by target warming
#remove exp02 (chuine), as these variances aren't real
alltemptarget<-alltemptarget[-which(alltemptarget$site=="exp02"),]

sitesymb<-c(21,22,16,24,25,17,3,4,5,10,12)
symb_site<-sitesymb[factor(alltemptarget$site)]

quartz(height=5,width=10)
par(mfrow=c(2,2),mai=c(.3,.6,.2,.05),omi=c(.5,.5,.2,.5))
plot(alltemptarget$target,alltemptarget$AGTempMax_Var,pch=symb_site,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(20,180), bty="l", cex.axis=.9, main="Max AG temp")
#mtext(side=2,"Max AG temp", line=2.2, adj=.8, cex=.9)
axis(side=2,at=c(50,100,150), labels=TRUE, las=TRUE, cex=.9)
axis(side=1,at=c(-1,0,1,2,3,4,5), labels=FALSE,cex=.9)
mtext(side=2,"Variance (C)", line=3,adj=-1,cex=.9)
plot(alltemptarget$target,alltemptarget$AGTempMin_Var,pch=symb_site,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(20,180), bty="l",, cex.axis=.9, main="Min AG temp")
axis(side=2,at=c(50,100,150), labels=TRUE, las=TRUE, cex=.9)
axis(side=1,at=c(-1,0,1,2,3,4,5), labels=FALSE,cex=.9)
#mtext(side=2,"Min AG temp", line=1, adj=.8, cex=.9)
plot(alltemptarget$target,alltemptarget$BGTempMax_Var,pch=symb_site,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(20,180), bty="l", cex.axis=.9,main="Max BG temp")
axis(side=2,at=c(50,100,150), labels=TRUE, las=TRUE, cex=.9)
axis(side=1,at=c(-1,0,1,2,3,4,5), labels=c("ambient","0","1","2","3","4","5"),cex=.9)
mtext(side=1,"Target warming (C)", line=2.3, adj=.5)
plot(alltemptarget$target,alltemptarget$BGTempMin_Var,pch=symb_site,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(20,180), bty="l",cex.axis=.9, main="Min BG temp")
axis(side=1,at=c(-1,0,1,2,3,4,5), labels=c("ambient","0","1","2","3","4","5"),cex=.9)
axis(side=2,at=c(50,100,150), labels=TRUE, las=TRUE, cex=.9)
mtext(side=1,"Target warming (C)", line=2.3, adj=.5)
legend(x=4.5,y=195,legend=unique(alltemptarget$site),pch=unique(symb_site),bty="n",cex=0.7,pt.cex=0.7)

#remake figure with site/experiment on x axis, and color coding by target warming
targetcol<-c("black","gray","white","#FFF5F0","#FEE0D2","#FCBBA1","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")

quartz(height=5,width=10)
par(mfrow=c(2,2),mai=c(.3,.6,.2,.05),omi=c(.5,.5,.2,.5))
plot(jitter(as.numeric(as.factor(alltemptarget$site))),alltemptarget$AGTempMax_Var,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(20,180), bty="l", cex.axis=.9, main="Max AG temp",pch = 21, bg = c(targetcol[as.factor(as.character(alltemptarget$target))]))
axis(side=2,at=c(50,100,150), labels=TRUE, las=TRUE, cex=.9)
axis(side=1,at=c(seq(1:11)), labels=FALSE,cex=.9)
mtext(side=2,"Variance (C)", line=3,adj=-1,cex=.9)
plot(jitter(as.numeric(as.factor(alltemptarget$site))),alltemptarget$AGTempMin_Var,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(20,180), bty="l", cex.axis=.9, main="Min AG temp",pch = 21, bg = c(targetcol[as.factor(as.character(alltemptarget$target))]))
axis(side=2,at=c(50,100,150), labels=TRUE, las=TRUE, cex=.9)
axis(side=1,at=c(seq(1:11)), labels=FALSE,cex=.9)
plot(jitter(as.numeric(as.factor(alltemptarget$site))),alltemptarget$BGTempMax_Var,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(20,180), bty="l", cex.axis=.9, main="Max BG temp",pch = 21, bg = c(targetcol[as.factor(as.character(alltemptarget$target))]))
axis(side=1,at=c(seq(1:11)), labels=substr(sort(unique(alltemptarget$site)),4,5),cex=.9)
axis(side=2,at=c(50,100,150), labels=TRUE, las=TRUE, cex=.9)

mtext(side=1,"Study/site", line=2.3, adj=.5)
plot(jitter(as.numeric(as.factor(alltemptarget$site))),alltemptarget$BGTempMin_Var,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(20,180), bty="l", cex.axis=.9, main="Min BG temp",pch = 21, bg = c(targetcol[as.factor(as.character(alltemptarget$target))]))
axis(side=1,at=c(seq(1:11)), labels=substr(sort(unique(alltemptarget$site)),4,5),cex=.9)
axis(side=2,at=c(50,100,150), labels=TRUE, las=TRUE, cex=.9)
mtext(side=1,"Study/site", line=2.3, adj=.5)
legend(x=10.5,y=195,legend=sort(unique(alltemptarget$target)),pch=21,pt.bg=targetcol,bty="n",cex=0.7,pt.cex=0.7)

alltemptarget$treatcat<-NA
alltemptarget[which(alltemptarget$temptreat=="ambient"|alltemptarget$temptreat==0),]$treatcat<-"control"
alltemptarget[which(as.numeric(alltemptarget$temptreat)>0),]$treatcat<-"warmed"

head(alltemptarget)
library(lme4)
alltemptarget$site<-as.factor(alltemptarget$site)
alltemptarget$treatcat<-as.factor(alltemptarget$treatcat)
#testing if treatments yield larger variances.
test1<-lmer(AGTempMax_Var~treatcat + (1|site), data=alltemptarget,na.action=na.omit)
summary(test1)
test2<-lmer(AGTempMin_Var~treatcat + (1|site), data=alltemptarget,na.action=na.omit)
summary(test2)
test3<-lmer(BGTempMax_Var~treatcat + (1|site), data=alltemptarget,na.action=na.omit)
summary(test3)
test4<-lmer(BGTempMin_Var~treatcat + (1|site), data=alltemptarget,na.action=na.omit)
summary(test4)#

#all show a trend toward higher variance in the warmed vs unwarmed but doesn't seem to be
#"significant (small t value). BG min is that only one that may be significant
#But, i realize that Yann was actually interested in comparing the variance of min vs max temp, so i should restructure these models a bit
#(figures are fine)
maxtemp<-cbind(subset(alltemptarget,select=c(site,target,treatcat,AGTempMax_Var,BGTempMax_Var)),rep("tmax", times=dim(alltemptarget)[1]))
colnames(maxtemp)<-c("site","target","treatcat","AGVar","BGVar","temptype")
mintemp<-cbind(subset(alltemptarget,select=c(site,target,treatcat,AGTempMin_Var,BGTempMin_Var)),rep("tmin", times=dim(alltemptarget)[1]))
colnames(mintemp)<-c("site","target","treatcat","AGVar","BGVar","temptype")

newdat<-rbind(maxtemp,mintemp)
test5<-lmer(AGVar~treatcat*temptype + (1|site), data=newdat,na.action=na.omit)
summary(test5)
test6<-lmer(BGVar~treatcat*temptype + (1|site), data=newdat,na.action=na.omit)
summary(test6)#


####Try coefficient of variation for each treatment and temperature type
head(expclimt)#may need to aggregate instead,,,
cv_agtemp_max<-aggregate(expclimt$agtemp_max, by=list(expclimt$site,expclimt$plot,expclimt$temptreat), FUN=cv,na.rm=TRUE)
colnames(cv_agtemp_max)<-c("site","plot","temptreat","agtemp_max")

#now Min AG temp
cv_agtemp_min<-aggregate(expclimt$agtemp_min, by=list(expclimt$site,expclimt$plot,expclimt$temptreat), FUN=cv,na.rm=TRUE)
colnames(cv_agtemp_min)<-c("site","plot","temptreat","agtemp_min")

#BG Max Temp
cv_bgtemp_max<-aggregate(expclimt$soiltemp1_max, by=list(expclimt$site,expclimt$plot,expclimt$temptreat), FUN=cv,na.rm=TRUE)
colnames(cv_bgtemp_max)<-c("site","plot","temptreat","bgtemp_max")

#BG Min Temp
cv_bgtemp_min<-aggregate(expclimt$soiltemp1_min, by=list(expclimt$site,expclimt$plot,expclimt$temptreat), FUN=cv,na.rm=TRUE)
colnames(cv_bgtemp_min)<-c("site","plot","temptreat","bgtemp_min")

#Now combine the four temperature variables
dim(cv_bgtemp_min);dim(cv_bgtemp_max);dim(cv_agtemp_min);dim(cv_agtemp_max)
#Add new column for temptreat that can be merged with the files so that it has target warming instead of level
cv_all<-cbind(cv_agtemp_max,cv_agtemp_min[,4],cv_bgtemp_max[,4],cv_bgtemp_min[,4])
colnames(cv_all)[4:7]<-c("cv_agtemp_max","cv_agtemp_min","cv_bgtemp_max","cv_bgtemp_min")
colnames(cv_all)[3]<-"temptreatx"
cv_all$temptreat<-NA
cv_all[which(cv_all$temptreatx=="ambient"),]$temptreat<-cv_all[which(cv_all$temptreatx=="ambient"),]$temptreatx
cv_all[1:130,]$temptreat<-substr(cv_all$temptreatx[1:130],1,2)
#now merge target temperatures in
cv_allt <- left_join(treats2,cv_all, by=c("site","temptreat"), match="all")
cv_allt[which(cv_allt$temptreatx=="ambient"),]$target<--1
unique(cv_allt$temptreat)
#now figure
#remake figure with site/experiment on x axis, and color coding by target warming
targetcol<-c("black","gray","white","#FFF5F0","#FEE0D2","#FCBBA1","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
quartz(height=5,width=10)
par(mfrow=c(2,2),mai=c(.3,.6,.2,.05),omi=c(.5,.5,.2,.5))
plot(as.numeric(as.factor(cv_allt$site)),cv_allt$cv_agtemp_max,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(0,100), bty="l", cex.axis=.9, main="Max AG Temp CV",pch = 21, bg = c(targetcol[as.factor(as.character(cv_allt$target))]))
axis(side=2,at=c(0,50,100), labels=TRUE, las=TRUE, cex=.9)
axis(side=1,at=c(seq(1:11)), labels=FALSE,cex=.9)
mtext(side=2,"CV (C)", line=3,adj=-1,cex=.9)
plot(as.numeric(as.factor(cv_allt$site)),cv_allt$cv_agtemp_min,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(0,400), bty="l", cex.axis=.9, main="Min AG temp",pch = 21, bg = c(targetcol[as.factor(as.character(cv_allt$target))]))
axis(side=2,at=c(0,100,200,300,400), labels=TRUE, las=TRUE, cex=.9)
axis(side=1,at=c(seq(1:11)), labels=FALSE,cex=.9)
plot(as.numeric(as.factor(cv_allt$site)),cv_allt$cv_bgtemp_max,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(0,100), bty="l", cex.axis=.9, main="Max BG temp",pch = 21, bg = c(targetcol[as.factor(as.character(cv_allt$target))]))
axis(side=1,at=c(seq(1:12)), labels=substr(sort(unique(cv_allt$site)),4,5),cex=.9)
axis(side=2,at=c(0,50,100), labels=TRUE, las=TRUE, cex=.9)

mtext(side=1,"Study/site", line=2.3, adj=.5)
plot(as.numeric(as.factor(cv_allt$site)),cv_allt$cv_bgtemp_min,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(0,100), bty="l", cex.axis=.9, main="Min BG temp",pch = 21, bg = c(targetcol[as.factor(as.character(cv_allt$target))]))

axis(side=1,at=c(seq(1:12)), labels=substr(sort(unique(cv_allt$site)),4,5),cex=.9)
axis(side=2,at=c(0,50,100), labels=TRUE, las=TRUE, cex=.9)
mtext(side=1,"Study/site", line=2.3, adj=.5)
legend(x=11.5,y=100,legend=sort(unique(cv_allt$target)),pch=21,pt.bg=targetcol,bty="n",cex=0.7,pt.cex=0.7)


#Make same plots, but by month for each site

####Try coefficient of variation for each treatment and temperature type
head(expclimt)#may need to aggregate instead,,,
expclimt<-expclim2[which(expclim2$preciptreat==0|is.na(expclim2$preciptreat)),]
expclimt$agtemp_min<-expclimt$airtemp_min
expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$cantemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$airtemp_min) & !is.na(expclimt$cantemp_min)),]$cantemp_min
expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$surftemp_min
expclimt$agtemp_max<-expclimt$airtemp_max
expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$cantemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$airtemp_max) & !is.na(expclimt$cantemp_max)),]$cantemp_max
expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$surftemp_max
#add column for month to expclimt
expclimt$month<-substr(as.Date(paste(expclimt$year, expclimt$doy,sep="-"), format="%Y-%j"),6,7)
expclimt <- left_join(treats2,expclimt, by=c("site","temptreat"), match="all")

sites<-sort(unique(expclimt$site))#use data without precipitation manipulation
for(i in i:length(sites))
{
  sited<-expclimt[expclimt$site==sites[i],]
  #Max AG Temp
  cv_agtemp_max<-aggregate(expclimt$agtemp_max, by=list(expclimt$month,expclimt$plot,expclimt$temptreat,expclimt$target.x), FUN=cv,na.rm=TRUE)
  colnames(cv_agtemp_max)<-c("month","plot","temptreat","target","agtemp_max")
  #now Min AG temp
  cv_agtemp_min<-aggregate(expclimt$agtemp_min, by=list(expclimt$month,expclimt$plot,expclimt$temptreat,expclimt$target.x), FUN=cv,na.rm=TRUE)
  colnames(cv_agtemp_min)<-c("month","plot","temptreat","target","agtemp_min")
  
  #BG Max Temp
  cv_bgtemp_max<-aggregate(expclimt$soiltemp1_max, by=list(expclimt$month,expclimt$plot,expclimt$temptreat,expclimt$target.x), FUN=cv,na.rm=TRUE)
  colnames(cv_bgtemp_max)<-c("month","plot","temptreat","target","bgtemp_max")
  
  #BG Min Temp
  cv_bgtemp_min<-aggregate(expclimt$soiltemp1_min, by=list(expclimt$month,expclimt$plot,expclimt$temptreat,expclimt$target.x), FUN=cv,na.rm=TRUE)
  colnames(cv_bgtemp_min)<-c("month","plot","temptreat","target","bgtemp_min")
  #Now combine the four temperature variables
  #Add new column for temptreat that can be merged with the files so that it has target warming instead of level
  cv_all<-cbind(cv_agtemp_max,cv_agtemp_min[,5],cv_bgtemp_max[,5],cv_bgtemp_min[,5])
  colnames(cv_all)[5:8]<-c("cv_agtemp_max","cv_agtemp_min","cv_bgtemp_max","cv_bgtemp_min")
  colnames(cv_all)[3]<-"temptreatx"
  cv_all$temptreat<-cv_all$temptreatx
  #now merge target temperatures in
  if(length(which(cv_all$temptreatx=="ambient"))>0){cv_all[which(cv_all$temptreatx=="ambient"),]$target<--1}
  #now figures
  #remake figure with site/experiment on x axis, and color coding by target warming
  targetcol<-c("black","gray","white","#FFF5F0","#FEE0D2","#FCBBA1","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  targetcol<-targetcol[1:length(cvall$)]
  quartz(height=5,width=10)
  par(mfrow=c(2,2),mai=c(.3,.6,.2,.05),omi=c(.5,.5,.2,.5))
  plot(as.numeric(as.factor(cv_all$month)),cv_all$cv_agtemp_max,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(0,100), bty="l", cex.axis=.9, main=paste(sites[i],"AG Max"),pch = 21, bg = c(targetcol[as.factor(as.character(cv_all$target))]))
  axis(side=2,at=c(0,50,100), labels=TRUE, las=TRUE, cex=.9)
  axis(side=1,at=c(seq(1:11)), labels=FALSE,cex=.9)
  mtext(side=2,"CV (C)", line=3,adj=-1,cex=.9)
  plot(as.numeric(as.factor(cv_all$month)),cv_all$cv_agtemp_min,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(0,400), bty="l", cex.axis=.9, main=paste(sites[i],"AG Min"),pch = 21, bg = c(targetcol[as.factor(as.character(cv_allt$target))]))
  axis(side=2,at=c(0,100,200,300,400), labels=TRUE, las=TRUE, cex=.9)
  axis(side=1,at=c(seq(1:11)), labels=FALSE,cex=.9)
  plot(as.numeric(as.factor(cv_all$month)),cv_all$cv_bgtemp_max,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(0,100), bty="l", cex.axis=.9, main=paste(sites[i],"BG Max"),pch = 21, bg = c(targetcol[as.factor(as.character(cv_allt$target))]))
  axis(side=1,at=c(seq(1:12)), labels=substr(sort(unique(cv_all$month)),4,5),cex=.9)
  axis(side=2,at=c(0,50,100), labels=TRUE, las=TRUE, cex=.9)
  
  mtext(side=1,"Month", line=2.3, adj=.5)
  plot(as.numeric(as.factor(cv_all$month)),cv_all$cv_bgtemp_min,xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(0,100), bty="l", cex.axis=.9, main=paste(sites[i],"BG Min"),pch = 21, bg = c(targetcol[as.factor(as.character(cv_allt$target))]))
  
  axis(side=1,at=c(seq(1:12)), labels=substr(sort(unique(cv_all$month)),4,5),cex=.9)
  axis(side=2,at=c(0,50,100), labels=TRUE, las=TRUE, cex=.9)
  mtext(side=1,"Month", line=2.3, adj=.5)
  legend(x=11.5,y=100,legend=sort(unique(cv_all$target)),pch=21,pt.bg=targetcol,bty="n",cex=0.7,pt.cex=0.7)
}

