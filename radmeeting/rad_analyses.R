##Preliminary analyses of exp and obs data for radcliff
##March 18, 2016
##Read in climate and phenology data
setwd("~/GitHub/radcliffe/radmeeting")
#obsclim<-read.csv("obsclim.csv", header=T)
expclim<-read.csv("expclim.csv", header=T)
exppheno<-read.csv("exppheno.csv", header=T)
obspheno<-read.csv("obspheno.csv", header=T)
head(expclim)

expmat<-aggregate(x=subset(expclim, select=c("soiltemp1_mean")), by=list(expclim$year,expclim$site,expclim$plot), FUN=mean, na.rm=T)
colnames(expmat)[1:3]<-c("year","site","plot")
dim(expmat)
expdat<-merge(expmat,exppheno)
expdat$spgen<-paste(expdat$genus,expdat$species,sep=".")

library(lme4)
obs.mod<-lmer(doy ~ tmean + (1|site/spgen), data=obsdat)
summary(obs.mod)
ranef(obs.mod)

head(expclim)
exp.mod<-lmer(doy ~ soiltemp1_mean + (1|site/spgen), data=expdat)
exp.CI<-confint(exp.mod)
obs.CI<-confint(obs.mod)

summary(exp.mod)
quartz()
plot(2,fixef(obs.mod)[2],type="p", pch=22,cex=1.3,col="blue",bty="l",ylab="Sensitivity (change days per C)", xaxt="n", xlim=c(.5,2.5), ylim=c(-7.5,0))
points(1,fixef(exp.mod)[2],type="p", pch=16, cex=1.3,col="darkred") 
legend(.5,-.5,legend=c("Experiments","Observations"), bty="n",pch=c(16,22),col=c("darkred","blue"))
abline(h=0, lty=2, lwd=2, col="gray")
arrows(1,-1.973586,1,-2.249826,length=.05,angle=90,col="darkred",code=3,lwd=2)
arrows(2,-7.177654,2,-6.953411,length=.05,angle=90,col="blue",code=3,lwd=2)


leaf.expdat<-expdat[expdat$event=="lod"|expdat$event=="bbd"|expdat$event=="lud",]
leaf.obsdat<-obsdat[obsdat$event=="lod"|obsdat$event=="bbd"|obsdat$event=="lud",]
flow.expdat<-expdat[expdat$event=="ffd"|expdat$event=="ffrd",]
flow.obsdat<-obsdat[obsdat$event=="ffd"|obsdat$event=="ffrd",]
leaf.obs.mod<-lmer(doy ~ tmean + (1|site/spgen), data=leaf.obsdat)
summary(leaf.obs.mod)
flow.obs.mod<-lmer(doy ~ tmean + (1|site/spgen), data=flow.obsdat)
summary(flow.obs.mod)
leaf.exp.mod<-lmer(doy ~ soiltemp1_mean + (1|site/spgen), data=leaf.expdat)
summary(leaf.obs.mod)
flow.exp.mod<-lmer(doy ~ soiltemp1_mean + (1|spgen), data=flow.expdat)
summary(flow.exp.mod)

head(expclim)
exp.mod<-lmer(doy ~ soiltemp1_mean + (1|site/spgen), data=expdat)
leaf.obs.CI<-confint(leaf.obs.mod)
flow.obs.CI<-confint(flow.obs.mod)
leaf.exp.CI<-confint(leaf.exp.mod)
flow.exp.CI<-confint(flow.exp.mod)

quartz()
plot(c(1.25,2.25),c(fixef(flow.obs.mod)[2],fixef(leaf.obs.mod)[2]),type="p", pch=22,cex=1.3,col="blue",bty="l",ylab="Sensitivity (change days per C)", xaxt="n", xlab="",xlim=c(.5,2.5), ylim=c(-7.5,0), cex.lab=1.5, cex.axis=1.2)
points(c(.75,1.75),c(fixef(flow.exp.mod)[2],fixef(leaf.exp.mod)[2]),type="p", pch=16, cex=1.3,col="darkred") 
legend(.5,-.5,legend=c("Experiments","Observations"), bty="n",pch=c(16,22),col=c("darkred","blue"))
abline(h=0, lty=2, lwd=2, col="gray")
arrows(1.25,leaf.obs.CI[5,1],1.25,leaf.obs.CI[5,2],length=.05,angle=90,col="blue",code=3,lwd=2)
arrows(2.25,flow.obs.CI[5,1],2.25,flow.obs.CI[5,2],length=.05,angle=90,col="blue",code=3,lwd=2)
arrows(.75,flow.exp.CI[4,1],.75,flow.exp.CI[4,2],length=.05,angle=90,col="darkred",code=3,lwd=2)
arrows(1.75,leaf.exp.CI[5,1],1.75,leaf.exp.CI[5,2],length=.05,angle=90,col="darkred",code=3,lwd=2)
mtext("Flowering", side=1,line=1, adj=0.25, cex=1.2)
mtext("Leafing", side=1,line=1, adj=0.75, cex=1.2)

#make some plots of the climate to compare:
quartz()
par(mfrow=c(1,3))
hist(expclim$soiltemp1_mean, main ="Experiments",xlab="Mean soil temp",ylim=c(1,180000))
hist((expclim$airtemp_min+expclim$airtemp_max)/2, main ="Experiments",xlab="Mean air temp",ylim=c(1,180000)) 
hist(obsclim$tmean, main="Observations",xlab="Mean annual temp",ylim=c(1,180000))

quartz()
par(mfrow=c(1,2))
hist(flow.expdat$doy, main ="Experiments",xlab="Flowering DOY",ylim=c(1,50000))
hist(flow.obsdat$doy, main="Observations",xlab="Flowering DOY",ylim=c(1,50000))

quartz()
par(mfrow=c(1,2))
hist(leaf.expdat$doy, main ="Experiments",xlab="Leafing DOY",ylim=c(1,7000))
hist(leaf.obsdat$doy, main="Observations",xlab="Leafing DOY",ylim=c(1,7000))
