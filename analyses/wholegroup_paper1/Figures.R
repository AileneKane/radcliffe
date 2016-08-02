# ------------------------------------------
# Mock figures showing spatial variation in experimental warming
# A. Ettinger, aettinger@fas.harvard.edu
# Description: Mock plot of spatial variation: blocks vs plots vs treatment levels

setwd("~/git/radcliffe")
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Load packages
library(lme4)

# Read in experimental climate data
expclim<-read.csv("analyses/expclim.csv", header=T)
expclim_control<-expclim[expclim$temptreat=="0", ]#these are sham/true controls
expclim_control$temptreat<-factor(expclim_control$temptreat)
expclim_control$site<-as.factor(expclim_control$site)
expclim_control$block<-as.factor(expclim_control$block)
expclim_control$plot<-as.factor(expclim_control$plot)
boxplot(expclim_control$airtemp_min~expclim_control$plot)
##just look at sites with blocks AND plots
expclim_block<-expclim[which(!is.na(expclim$block)),]
expclim_block<-expclim_block[-which(expclim$temptreat=="ambient"),]

mod<-lmer(soiltemp1_mean~temptreat+(1|site/plot), data=expclim)
# Fit mixed model with just plot and block- temperature in control plots?
summary(mod)

boxplot(expclim$soiltemp1_mean~expclim$temptreat)
boxplot(expclim$soiltemp1_mean~expclim$plot)


#Ok, so those attempts went nowhere. 
#Here is a draft of the conceptual figure for biological implications of warming
quartz(width=11,height=6)
par(mfrow=c(2,3),omi=c(.1,2,.1,.1), mai=c(.6,.3,.5,.1))
x<-c(seq(1:10));y<-rev(seq(261,270))
plot(x,y,type="l", lwd=2, xlab="", ylab="", bty="l", cex.lab=1.2,ylim=c(260,280), yaxt="n",xaxt="n")

mtext("Muted response:",side=3, line=-7, adj=-1.5)
mtext("Direct Effect",side=3, line=2, adj=.5,cex=.9)
mtext("+",side=3, line=2, adj=1, cex=1.1)
mtext("Air temperature",side=1, line=1, adj=.5)
mtext("Focal Response",side=2, line=1.5)
mtext("(e.g. doy of phenological event)",side=2, line=.5, cex=.9)

y2<-seq(261,265.5,by=.5)
plot(x,y2,type="l", lwd=2, xlab="", ylab="", bty="l",cex.lab=1.2,ylim=c(260,280), yaxt="n",xaxt="n")
mtext("=",side=3, line=2, adj=1, cex=1.1)
mtext("Indirect Effect",side=3, line=2, adj=.5,cex=.9)
mtext("Artifically co-varying driver",side=1, line=1, adj=.5)
mtext("(e.g. soil moisture)",side=1, line=2, adj=.5,cex=.8)

y3<-rev(seq(261,265.5,by=.5))
plot(x,y3,type="l", lwd=2, xlab="", ylab="", bty="l",cex.lab=1.2,ylim=c(260,280), yaxt="n",xaxt="n")
mtext("Net Observed Effect of Warming",side=3, line=2, adj=.5,cex=.9)
mtext("Air temperature",side=1, line=1, adj=.5)

plot(x,y2,type="l", lwd=2, xlab="", ylab="", bty="l", cex.lab=1.1,ylim=c(260,280), yaxt="n",xaxt="n")
mtext("Exaggerated response:",side=3, line=-7, adj=-2.5)
mtext("Air temperature",side=1, line=1, adj=.5)
mtext("Focal Response",side=2, line=1.5)
mtext("(e.g. species abundance)",side=2, line=.5, cex=.9)

y5<-rev(seq(261,280,by=2))
plot(x,y5,type="l", lwd=2, xlab="", ylab="", bty="l", yaxt="n",xaxt="n")
mtext("=",side=3, line=1.5, adj=1.2)
mtext("Artifically co-varying driver",side=1, line=1, adj=.5)
mtext("(e.g. abundance of competing species)",side=1, line=2, adj=.5,cex=.8)

y5<-rev(seq(261,280,by=2))
plot(x,y2,type="l", lwd=2, xlab="", ylab="", bty="l", yaxt="n",xaxt="n")
mtext("Air temperature",side=1, line=1, adj=.5)
