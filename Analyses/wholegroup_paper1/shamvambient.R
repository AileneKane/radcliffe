###Analyses of sham ("control") vs. "ambient" microclimate data, to examine how warming chambers may alter micrclimate.
#Look at temperature, soil moisture
rm(list=ls()) 
options(stringsAsFactors = FALSE)


library(lme4)
setwd("~/git/radcliffe/Analyses")
#expclim<-read.csv("gddchill/expclim.wchillgdd.csv", header=T)
expclim<-read.csv("expclim.csv", header=T)
##want to plot "0" compared to "ambient" in each site
##select out just these two treatments for now
expclim_controls<-expclim[expclim$temptreat=="0"|expclim$temptreat=="ambient",]
expclim_controls$temptreat<-factor(expclim_controls$temptreat)
expclim_controls$airtemp_mean<-(expclim_controls$airtemp_min+expclim_controls$airtemp_max)/2
sitesums<-data.frame(tapply(expclim_controls$soiltemp1_mean,list(expclim_controls$site,expclim_controls$temptreat),length))
colnames(sitesums)<-c("sham.control","ambient")
sites_con<-rownames(sitesums)[!is.na(sitesums$sham.control) & !is.na(sitesums$ambient)]
expclim_cont<-expclim_controls[expclim_controls$site %in% sites_con,]
expclim_cont$year<-as.factor(expclim_cont$year)
expclim_cont$date<-strptime(paste(expclim_cont$year, expclim_cont$doy), "%Y%j")
expclim_cont$month<-substring(expclim_cont$date,6,7)
months<-sort(unique(expclim_cont$month))
monthsums_allyear<-c()
for (i in 1:length(months)){
  monthdat<-expclim_cont[expclim_cont$month==months[i],]
  monthdat$temptreat <- relevel(as.factor( monthdat$temptreat), ref = "ambient")
  soilmod<-lmer(soiltemp1_mean~temptreat + (temptreat|site/year), data= monthdat, REML=FALSE)
  airmod<-lmer(airtemp_mean~temptreat + (temptreat|site/year), data=monthdat, REML=FALSE)
  coefs_air<-data.frame(coef(summary(airmod)))
  monthsums_soil<-rbind(fixef(soilmod),coef(soilmod)$site)
  rownames(monthsums_soil)[1]<-"soil_fixed"
  SE<-c(data.frame(coef(summary(soilmod)))$Std[2],rep(NA, times=dim(monthsums_soil)[1]-1))
  t<-c(data.frame(coef(summary(soilmod)))$t.value[2],rep(NA, times=dim(monthsums_soil)[1]-1))
  numsites<-dim(ranef(soilmod)$site)[1]
  nsites<-c(numsites,rep(NA, times=dim(monthsums_soil)[1]-1))
  type<-c(rep("soil",times=dim(monthsums_soil)[1]))
  monthsums_soil<-cbind(type,monthsums_soil,SE,t,nsites)
  monthsums_air<-rbind(fixef(airmod),coef(airmod)$site)
  rownames(monthsums_air)[1]<-"air_fixed"
  SE<-c(data.frame(coef(summary(airmod)))$Std[2],rep(NA, times=dim(monthsums_air)[1]-1))
  t<-c(data.frame(coef(summary(airmod)))$t.value[2],rep(NA, times=dim(monthsums_air)[1]-1))
  numsites<-dim(ranef(airmod)$site)[1]
  nsites<-c(numsites,rep(NA, times=dim(monthsums_air)[1]-1))
  type<-c(rep("soil",times=dim(monthsums_soil)[1]))
  type<-c(rep("air",times=dim(monthsums_air)[1]))
  monthsums_air<-cbind(type,monthsums_air,SE,t,nsites)
  monthsums_all<-rbind(monthsums_soil,monthsums_air)
  month<-rep(months[i], times=dim(monthsums_all)[1])
  monthsums_all<-cbind(month,monthsums_all)
  monthsums_allyear<-rbind(monthsums_allyear,monthsums_all)
}
#Plot model results:
soil_monthsums<-monthsums_allyear[substring(rownames(monthsums_allyear),1,10)=="soil_fixed",]
air_monthsums<-monthsums_allyear[substring(rownames(monthsums_allyear),1,9)=="air_fixed",]

#Now, min soil and air temp
monthsums_allyear_min<-c()
soilranef_all<-data.frame(matrix(NA, nrow = 48, ncol = 4))
airranef_all<-data.frame(matrix(NA, nrow = 48, ncol = 4))
for (i in 1:length(months)){
  monthdat<-expclim_cont[expclim_cont$month==months[i],]
  monthdat$temptreat <- relevel(as.factor( monthdat$temptreat), ref = "ambient")
  soilmod<-lmer(soiltemp1_min~temptreat + (temptreat|site/year), data= monthdat, REML=FALSE)
  airmod<-lmer(airtemp_min~temptreat + (temptreat|site/year), data=monthdat, REML=FALSE)
  coefs_air<-data.frame(coef(summary(airmod)))
  monthsums_soil<-rbind(fixef(soilmod),coef(soilmod)$site)
  rownames(monthsums_soil)[1]<-"soil_fixed"
  SE<-c(data.frame(coef(summary(soilmod)))$Std[2],rep(NA, times=dim(monthsums_soil)[1]-1))
  t<-c(data.frame(coef(summary(soilmod)))$t.value[2],rep(NA, times=dim(monthsums_soil)[1]-1))
  numsites<-dim(ranef(soilmod)$site)[1]
  nsites<-c(numsites,rep(NA, times=dim(monthsums_soil)[1]-1))
  type<-c(rep("soil",times=dim(monthsums_soil)[1]))
  monthsums_soil<-cbind(type,monthsums_soil,SE,t,nsites)
  monthsums_air<-rbind(fixef(airmod),coef(airmod)$site)
  rownames(monthsums_air)[1]<-"air_fixed"
  SE<-c(data.frame(coef(summary(airmod)))$Std[2],rep(NA, times=dim(monthsums_air)[1]-1))
  t<-c(data.frame(coef(summary(airmod)))$t.value[2],rep(NA, times=dim(monthsums_air)[1]-1))
  numsites<-dim(ranef(airmod)$site)[1]
  nsites<-c(numsites,rep(NA, times=dim(monthsums_air)[1]-1))
  type<-c(rep("air",times=dim(monthsums_air)[1]))
  monthsums_air<-cbind(type,monthsums_air,SE,t,nsites)
  monthsums_all<-rbind(monthsums_soil,monthsums_air)
  month<-rep(months[i], times=dim(monthsums_all)[1])
  monthsums_all<-cbind(month,monthsums_all)
  monthsums_allyear_min<-rbind(monthsums_allyear_min,monthsums_all)
  soilranef_month<-cbind(rownames(coef(soilmod)$site),paste(i),coef(soilmod)$site)
  soilranef_all[((i*4)-3):(i*4),]<-soilranef_month
  airranef_month<-cbind(rownames(coef(airmod)$site),paste(i),coef(airmod)$site)
  airranef_all[((i*4)-3):(i*4),]<-airranef_month
}
minsoilranef<-soilranef_all
minairranef<-airranef_all
colnames(minsoilranef)<-c("site","month","ambT","shamdif")
colnames(minairranef)<-c("site","month","ambT","shamdif")

#Plot model results:
soil_monthsums<-monthsums_allyear_min[substring(rownames(monthsums_allyear_min),1,10)=="soil_fixed",]
air_monthsums<-monthsums_allyear_min[substring(rownames(monthsums_allyear_min),1,9)=="air_fixed",]

quartz(height=6,width=10)
par(mfcol=c(2,3),mai=c(.6,.7,.2,.1), omi=c(.7,.01,.2,.2))

#air
plot(as.numeric(air_monthsums$month),air_monthsums$temptreat0,type="p", pch=8,bg="black", xlab="", ylab="", ylim=c(-2,2),bty="l", main="Min Air Temp", las=TRUE)
#add random effects
minaexp03<-minairranef[which(minairranef$site=="exp03"),]
minaexp04<-minairranef[which(minairranef$site=="exp04"),]
minaexp07<-minairranef[which(minairranef$site=="exp07"),]
minaexp10<-minairranef[which(minairranef$site=="exp10"),]
points(as.numeric(minaexp03$month),minaexp03$shamdif,pch=21,bg="darkorchid",col="darkorchid")
points(as.numeric(minaexp04$month),minaexp04$shamdif,pch=22,bg="darkorchid",col="darkorchid")
points(as.numeric(minaexp07$month),minaexp07$shamdif,pch=22,bg="blue",col="blue")
points(as.numeric(minaexp10$month),minaexp10$shamdif,pch=21,bg="blue",col="blue")
for (i in 1:12){
  arrows(as.numeric(air_monthsums$month[i]),air_monthsums$temptreat0[i]-air_monthsums$SE[i],as.numeric(air_monthsums$month[i]),air_monthsums$temptreat0[i]+air_monthsums$SE[i],length=0.01,angle=90,code=3)}
abline(h=0,lty=2)
points(as.numeric(air_monthsums$month),air_monthsums$temptreat0,pch=8,bg="black")

mtext("Effect of structural control (sham - ambient)",side=2, line=3, cex=0.9,adj=1.1)
mtext(expression(~degree*C),side=2, line=2, cex=0.8,adj=.5)
mtext("a)",side=3, line=0, cex=0.9,adj=0)

legend(1.5,-.1,legend=c("exp03","exp04","exp07","exp08","exp10"), pch=c(21,22,22,22,21),pt.bg=c("darkorchid","darkorchid","blue","darkred","blue"),col=c("darkorchid","darkorchid","blue","darkred","blue"))

#soil
plot(as.numeric(soil_monthsums$month),soil_monthsums$temptreat0,type="p", pch=8,bg="black", xlab="", ylab="", ylim=c(-2,2), bty="l", main="Min Soil Temp", las=TRUE)
#add random effects
minsexp03<-minsoilranef[which(minsoilranef$site=="exp03"),]
minsexp04<-minsoilranef[which(minsoilranef$site=="exp04"),]
minsexp07<-minsoilranef[which(minsoilranef$site=="exp07"),]
minsexp10<-minsoilranef[which(minsoilranef$site=="exp10"),]
points(as.numeric(minsexp03$month),minsexp03$shamdif,pch=21,bg="darkorchid",col="darkorchid")
points(as.numeric(minsexp04$month),minsexp04$shamdif,pch=22,bg="darkorchid",col="darkorchid")
points(as.numeric(minsexp07$month),minsexp07$shamdif,pch=22,bg="blue",col="blue")
points(as.numeric(minsexp10$month),minsexp10$shamdif,pch=21,bg="blue",col="blue")

mtext("c)",side=3, line=0, cex=0.9,adj=0)
mtext(expression(~degree*C),side=2, line=2, cex=0.8,adj=.5)

for (i in 1:12){
  arrows(as.numeric(soil_monthsums$month[i]),soil_monthsums$temptreat0[i]-soil_monthsums$SE[i],as.numeric(soil_monthsums$month[i]),soil_monthsums$temptreat0[i]+soil_monthsums$SE[i],length=0.01,angle=90,code=3)}
abline(h=0,lty=2)
points(as.numeric(soil_monthsums$month),soil_monthsums$temptreat0,pch=8,bg="black")
mtext("Month",side=1, line=2, adj=.5,cex=.9)
##Now max soil and air temp
monthsums_allyear_max<-c()
soilranef_all<-data.frame(matrix(NA, nrow = 48, ncol = 4))
airranef_all<-data.frame(matrix(NA, nrow = 48, ncol = 4))
for (i in 1:length(months)){
  monthdat<-expclim_cont[expclim_cont$month==months[i],]
  monthdat$temptreat <- relevel(as.factor( monthdat$temptreat), ref = "ambient")
  soilmod<-lmer(soiltemp1_max~temptreat + (temptreat|site/year), data= monthdat, REML=FALSE)
  airmod<-lmer(airtemp_max~temptreat + (temptreat|site/year), data=monthdat, REML=FALSE)
  coefs_air<-data.frame(coef(summary(airmod)))
  monthsums_soil<-rbind(fixef(soilmod),coef(soilmod)$site)
  rownames(monthsums_soil)[1]<-"soil_fixed"
  SE<-c(data.frame(coef(summary(soilmod)))$Std[2],rep(NA, times=dim(monthsums_soil)[1]-1))
  t<-c(data.frame(coef(summary(soilmod)))$t.value[2],rep(NA, times=dim(monthsums_soil)[1]-1))
  numsites<-dim(ranef(soilmod)$site)[1]
  nsites<-c(numsites,rep(NA, times=dim(monthsums_soil)[1]-1))
  type<-c(rep("soil",times=dim(monthsums_soil)[1]))
  monthsums_soil<-cbind(type,monthsums_soil,SE,t,nsites)
  monthsums_air<-rbind(fixef(airmod),coef(airmod)$site)
  rownames(monthsums_air)[1]<-"air_fixed"
  SE<-c(data.frame(coef(summary(airmod)))$Std[2],rep(NA, times=dim(monthsums_air)[1]-1))
  t<-c(data.frame(coef(summary(airmod)))$t.value[2],rep(NA, times=dim(monthsums_air)[1]-1))
  numsites<-dim(ranef(airmod)$site)[1]
  nsites<-c(numsites,rep(NA, times=dim(monthsums_air)[1]-1))
  type<-c(rep("air",times=dim(monthsums_air)[1]))
  monthsums_air<-cbind(type,monthsums_air,SE,t,nsites)
  monthsums_all<-rbind(monthsums_soil,monthsums_air)
  month<-rep(months[i], times=dim(monthsums_all)[1])
  monthsums_all<-cbind(month,monthsums_all)
  monthsums_allyear_max<-rbind(monthsums_allyear_max,monthsums_all)
  soilranef_month<-cbind(rownames(coef(soilmod)$site),paste(i),coef(soilmod)$site)
  soilranef_all[((i*4)-3):(i*4),]<-soilranef_month
  airranef_month<-cbind(rownames(coef(airmod)$site),paste(i),coef(airmod)$site)
  airranef_all[((i*4)-3):(i*4),]<-airranef_month
}
maxsoilranef<-soilranef_all
maxairranef<-airranef_all
colnames(maxsoilranef)<-c("site","month","ambT","shamdif")
colnames(maxairranef)<-c("site","month","ambT","shamdif")
###Plot model results:
soil_monthsums<-monthsums_allyear_max[substring(rownames(monthsums_allyear_max),1,10)=="soil_fixed",]
air_monthsums<-monthsums_allyear_max[substring(rownames(monthsums_allyear_max),1,9)=="air_fixed",]
#air
plot(as.numeric(air_monthsums$month),air_monthsums$temptreat0,type="p", pch=8,bg="black", xlab="", ylab="", ylim=c(-2,2),bty="l", main="Max Air Temp", las=TRUE)
#add random effects
maxaexp03<-maxairranef[which(maxairranef$site=="exp03"),]
maxaexp04<-maxairranef[which(maxairranef$site=="exp04"),]
maxaexp07<-maxairranef[which(maxairranef$site=="exp07"),]
maxaexp10<-maxairranef[which(maxairranef$site=="exp10"),]
points(as.numeric(maxaexp03$month),maxaexp03$shamdif,pch=21,bg="darkorchid",col="darkorchid")
points(as.numeric(maxaexp04$month),maxaexp04$shamdif,pch=22,bg="darkorchid",col="darkorchid")
points(as.numeric(maxaexp07$month),maxaexp07$shamdif,pch=22,bg="blue",col="blue")
points(as.numeric(maxaexp10$month),maxaexp10$shamdif,pch=21,bg="blue",col="blue")

#error and fixed effects
abline(h=0,lty=2)
for (i in 1:12){
  arrows(as.numeric(air_monthsums$month[i]),air_monthsums$temptreat0[i]-air_monthsums$SE[i],as.numeric(air_monthsums$month[i]),air_monthsums$temptreat0[i]+air_monthsums$SE[i],length=0.01,angle=90,code=3)}
points(as.numeric(air_monthsums$month),air_monthsums$temptreat0,pch=8,bg="black")
mtext("b)",side=3, line=0, cex=0.9,adj=0)
mtext(expression(~degree*C),side=2, line=2, cex=0.8,adj=.5)

#soil
plot(as.numeric(soil_monthsums$month),soil_monthsums$temptreat0,type="p", pch=8,bg="black", xlab="", ylab="", ylim=c(-2,2), bty="l", main="Max Soil Temp", las=TRUE)
#add random effects
maxsexp03<-maxsoilranef[which(maxsoilranef$site=="exp03"),]
maxsexp04<-maxsoilranef[which(maxsoilranef$site=="exp04"),]
maxsexp07<-maxsoilranef[which(maxsoilranef$site=="exp07"),]
maxsexp10<-maxsoilranef[which(maxsoilranef$site=="exp10"),]
points(as.numeric(maxsexp03$month),maxsexp03$shamdif,pch=21,bg="darkorchid",col="darkorchid")
points(as.numeric(maxsexp04$month),maxsexp04$shamdif,pch=22,bg="darkorchid",col="darkorchid")
points(as.numeric(maxsexp07$month),maxsexp07$shamdif,pch=22,bg="blue",col="blue")
points(as.numeric(maxsexp10$month),maxsexp10$shamdif,pch=21,bg="blue",col="blue")

abline(h=0,lty=2)
for (i in 1:12){
  arrows(as.numeric(soil_monthsums$month[i]),soil_monthsums$temptreat0[i]-soil_monthsums$SE[i],as.numeric(soil_monthsums$month[i]),soil_monthsums$temptreat0[i]+soil_monthsums$SE[i],length=0.01,angle=90,code=3)}
points(as.numeric(soil_monthsums$month),soil_monthsums$temptreat0,pch=8,bg="black")
mtext("Month",side=1, line=2.5, adj=.5)
mtext("d)",side=3, line=0, cex=0.9,adj=0)
mtext(expression(~degree*C),side=2, line=2, cex=0.8,adj=.5)

expclim_cont2<-expclim_cont[-which(expclim_cont$site=="exp10"),]
moismod<-lmer(soilmois1~temptreat + (temptreat|site/year), data=expclim_cont2, REML=FALSE)
coefs_mois<-data.frame(coef(summary(moismod)))
moisranef_all<-data.frame(matrix(NA, nrow = 39, ncol = 4))
monthsums_allyear_mois<-c()
for (i in 1:length(months)){
  monthdat<-expclim_cont2[expclim_cont2$month==months[i],]
  monthdat$temptreat <- relevel(as.factor( monthdat$temptreat), ref = "ambient")
  moismod<-lmer(soilmois1~temptreat + (temptreat|site), data= monthdat, REML=FALSE)
  monthsums_soil<-rbind(fixef(moismod),coef(moismod)$site)
  rownames(monthsums_soil)[1]<-"mois_fixed"
  SE<-c(data.frame(coef(summary(moismod)))$Std[2],rep(NA, times=dim(monthsums_soil)[1]-1))
  t<-c(data.frame(coef(summary(moismod)))$t.value[2],rep(NA, times=dim(monthsums_soil)[1]-1))
  numsites<-dim(ranef(moismod)$site)[1]
  nsites<-c(numsites,rep(NA, times=dim(monthsums_soil)[1]-1))
  type<-c(rep("soil",times=dim(monthsums_soil)[1]))
  month<-rep(months[i], times=dim(monthsums_soil)[1])
  monthsums_soil<-cbind(month,type,monthsums_soil,SE,t,nsites)
  monthsums_allyear_mois<-rbind(monthsums_allyear_mois,monthsums_soil)
  moisranef_month<-cbind(rownames(coef(moismod)$site),paste(i),coef(moismod)$site)
  if(i<9){moisranef_all[((i*3)-2):(i*3),]<-moisranef_month}
  if(i==9){moisranef_all[((i*3)-2):((i*3)+1),]<-moisranef_month}
  if(i==10){moisranef_all[((i*3)-1):((i*3)+2),]<-moisranef_month}
  if(i==11){moisranef_all[(i*3):((i*3)+3),]<-moisranef_month}
  if(i==12){moisranef_all[((i*3)+1):((i*3)+3),]<-moisranef_month}
}
moisranef<-moisranef_all
colnames(moisranef)<-c("site","month","ambmois","shamdif")
###Plot soil moisture model results:
plot.new()
mois_monthsums<-monthsums_allyear_mois[substring(rownames(monthsums_allyear_mois),1,10)=="mois_fixed",]
plot(as.numeric(mois_monthsums$month),mois_monthsums$temptreat0,type="p", pch=8,bg="black", xlab="", ylab="", ylim=c(-.057,0.057),bty="l", main="Soil Moisture", las=TRUE)
moisexp03<-moisranef[which(moisranef$site=="exp03"),]
moisexp04<-moisranef[which(moisranef$site=="exp04"),]
moisexp07<-moisranef[which(moisranef$site=="exp07"),]
moisexp08<-moisranef[which(moisranef$site=="exp08"),]
points(as.numeric(moisexp03$month),moisexp03$shamdif,pch=21,bg="darkorchid",col="darkorchid")
points(as.numeric(moisexp04$month),moisexp04$shamdif,pch=22,bg="darkorchid",col="darkorchid")
points(as.numeric(moisexp07$month),moisexp07$shamdif,pch=22,bg="blue",col="blue")
points(as.numeric(moisexp08$month),moisexp08$shamdif,pch=22,bg="darkred",col="darkred")

abline(h=0,lty=2)
for (i in 1:12){
  arrows(as.numeric(mois_monthsums$month[i]),mois_monthsums$temptreat0[i]-mois_monthsums$SE[i],as.numeric(mois_monthsums$month[i]),mois_monthsums$temptreat0[i]+mois_monthsums$SE[i],length=0.01,angle=90,code=3)}
points(as.numeric(mois_monthsums$month),mois_monthsums$temptreat0,pch=8,bg="black")
mtext("Month",side=1, line=2.5, adj=.5)
mtext("VWC",side=2, line=2.5,adj=.5, cex=0.8)
mtext("e)",side=3, line=0, cex=0.9,adj=0)

#save estimates in a csv
#write.csv(monthsums_allyear,"output/shamVSambient_meandifs.csv",row.names=FALSE)
#write.csv(monthsums_allyear_max,"output/shamVSambient_maxdifs.csv",row.names=FALSE)
#write.csv(monthsums_allyear_min,"output/shamVSambient_mindifs.csv",row.names=FALSE)
