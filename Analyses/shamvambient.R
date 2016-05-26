###Analyses of sham ("control") vs. "ambient" microclimate data, to examine how warming chambers may alter micrclimate.
#Look at temperature, soil moisture
library(lme4)
setwd("~/GitHub/radcliffe/Analyses")
expclim<-read.csv("gddchill/expclim.wchillgdd.csv", header=T)
head(expclim)
unique(expclim$temptreat)
##want to plot "0" compared to "ambient" in each site
##select out just these two treatments for now
expclim_controls<-expclim[expclim$temptreat=="0" | expclim$temptreat=="ambient", ]
expclim_controls$temptreat<-factor(expclim_controls$temptreat)

unique(expclim_controls$site)

sitesums<-data.frame(tapply(expclim_controls$soiltemp1_mean,list(expclim_controls$site,expclim_controls$temptreat),length))
colnames(sitesums)<-c("sham.control","ambient")
sites_con<-rownames(sitesums)[!is.na(sitesums$sham.control) & !is.na(sitesums$ambient)]
sites_con<-sites_con[-which(sites_con=="chuine")]
expclim_cont<-expclim_controls[expclim_controls$site %in% sites_con,]
unique(expclim_cont$site)
head(expclim_cont)

#make figures of sham controls vs. ambient conditions:
#mean soil temp:
quartz(height=4, width=8)
par(mfrow=c(1,5))
for (i in 1:length(sites_con)){
  dat<-expclim_cont[expclim_cont$site==sites_con[i],]
  if(length(unique(dat$soiltemp1_mean))==1){next}
  boxplot(dat$soiltemp1_mean~dat$temptreat, main=paste(sites_con[i]), ylab="Mean soil temp")
}
mod2<-lmer(soiltemp1_mean~temptreat + (1|site), data=expclim_cont, REML=FALSE)
coefs2<-data.frame(coef(summary(mod2)))

quartz(height=4, width=8)
par(mfrow=c(1,5))
for (i in 1:length(sites_con)){
  dat<-expclim_cont[expclim_cont$site==sites_con[i],]
  if(length(unique(dat$soiltemp1_min))==1){next}
  boxplot(dat$soiltemp1_min~dat$temptreat, main=paste(sites_con[i]), ylab="Min soil temp")
}
mod3<-lmer(soiltemp1_min~temptreat + (1|site), data=expclim_cont, REML=FALSE)
coefs3<-data.frame(coef(summary(mod3)))

quartz(height=4, width=8)
par(mfrow=c(1,5))
for (i in 1:length(sites_con)){
  dat<-expclim_cont[expclim_cont$site==sites_con[i],]
  if(length(unique(dat$soiltemp1_max))==1){next}
  boxplot(dat$soiltemp1_max~dat$temptreat, main=paste(sites_con[i]), ylab="M soil temp")
}
mod4<-lmer(soiltemp1_max~temptreat + (1|site), data=expclim_cont, REML=FALSE)
coefs4<-data.frame(coef(summary(mod4)))
soil_difs<-c(coefs2[2,1],coefs4[2,1],coefs3[2,1])
#looks like soil temperatures are colder in the shams
####Now mean air temp (for the 4 sites that have this)
quartz(height=4, width=8)
par(mfrow=c(1,4))
for (i in 1:length(sites_con)){
  dat<-expclim_cont[expclim_cont$site==sites_con[i],]
  if(length(unique(dat$airtemp_mean))==1){next}
  boxplot(dat$airtemp_mean~dat$temptreat, main=paste(sites_con[i]), ylab="Mean air temp")
}
quartz(height=4, width=8)
par(mfrow=c(1,4))
for (i in 1:length(sites_con)){
  dat<-expclim_cont[expclim_cont$site==sites_con[i],]
  if(length(unique(dat$airtemp_min))==1){next}
  boxplot(dat$airtemp_min~dat$temptreat, main=paste(sites_con[i]), ylab="Min air temp")
}
quartz(height=4, width=8)
par(mfrow=c(1,4))
for (i in 1:length(sites_con)){
  dat<-expclim_cont[expclim_cont$site==sites_con[i],]
  if(length(unique(dat$airtemp_max))==1){next}
  boxplot(dat$airtemp_max~dat$temptreat, main=paste(sites_con[i]), ylab="Max air temp")
}
#looks like air temperatures are WARMER in the shams
expclim_cont$temptreat <- relevel(as.factor(expclim_cont$temptreat), ref = "ambient")
mod_air<-lmer(airtemp_mean~temptreat + (1|site), data=expclim_cont, REML=FALSE)
coefs_air<-data.frame(coef(summary(mod_air)))
print(coef(summary(mod_air)))
#Do we see the same patters for max and min values? OR are these altered in different ways 
mod_max<-lmer(airtemp_max~temptreat + (1|site), data=expclim_cont, REML=FALSE)
coefs_max<-data.frame(coef(summary(mod_max)))
mod_min<-lmer(airtemp_min~temptreat + (1|site), data=expclim_cont, REML=FALSE)
coefs_min<-data.frame(coef(summary(mod_min)))
coefs_max
coefs_min
air_difs<-c(coefs_air[2,1], coefs_min[2,1], coefs_max[2,1])
amb_shamdifs<-rbind(round(soil_difs, digits=3),round(air_difs, digits=3))
colnames(amb_shamdifs)<-c("mean","min","max")
rownames(amb_shamdifs)<-c("soiltemp","airtemp")
##look at differences by site, to see if there are relationships by warming type. perhaps add this interaction?
ranef(mod2)
fixef(mod2)
coef(mod2)
#to do this, include random slope too:
expclim_cont$temptreat <- relevel(as.factor(expclim_cont$temptreat), ref = "ambient")
mod2a<-lmer(soiltemp1_mean~temptreat + (temptreat|site), data=expclim_cont, REML=FALSE)
summary(mod2a)
coef(mod2a)
mod_air<-lmer(airtemp_mean~temptreat + (temptreat|site), data=expclim_cont, REML=FALSE)
coefs_air<-data.frame(coef(summary(mod_air)))
summary(mod_air)
coef(mod_air)
###Ok, now look across season-make plots and fit models by month.
###First, for mean soil and air:
expclim_cont$date<-strptime(paste(expclim_cont$year, expclim_cont$doy), "%Y%j")
expclim_cont$month<-substring(expclim_cont$date,6,7)
months<-sort(unique(expclim_cont$month))
monthsums_allyear<-c()
for (i in 1:length(months)){
  monthdat<-expclim_cont[expclim_cont$month==months[i],]
  monthdat$temptreat <- relevel(as.factor( monthdat$temptreat), ref = "ambient")
  soilmod<-lmer(soiltemp1_mean~temptreat + (temptreat|site), data= monthdat, REML=FALSE)
  airmod<-lmer(airtemp_mean~temptreat + (temptreat|site), data=monthdat, REML=FALSE)
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
###Plot model results:
soil_monthsums<-monthsums_allyear[substring(rownames(monthsums_allyear),1,10)=="soil_fixed",]
air_monthsums<-monthsums_allyear[substring(rownames(monthsums_allyear),1,9)=="air_fixed",]

quartz(height=6,width=7)
par(mfrow=c(2,1),mai=c(.6,.7,.2,.1), omi=c(.7,.01,.2,.2))
#soil
plot(as.numeric(soil_monthsums$month),soil_monthsums$temptreat0,type="p", pch=21,bg="brown4", xlab="Month", ylab="", ylim=c(min(soil_monthsums$temptreat0)-(max(soil_monthsums$SE)),max(soil_monthsums$temptreat0)+(max(soil_monthsums$SE))), bty="l", main="Mean Soil Temp")
for (i in 1:12){
  arrows(as.numeric(soil_monthsums$month[i]),soil_monthsums$temptreat0[i]-soil_monthsums$SE[i],as.numeric(soil_monthsums$month[i]),soil_monthsums$temptreat0[i]+soil_monthsums$SE[i],length=0.05,angle=90,code=3)}
points(as.numeric(soil_monthsums$month),soil_monthsums$temptreat0,pch=21,bg="brown4")
#air
mtext("Difference between sham and ambient",side=2, line=2, adj=1.8)
plot(as.numeric(air_monthsums$month),air_monthsums$temptreat0,type="p", pch=21,bg="blue", xlab="Month", ylab="", ylim=c(min(air_monthsums$temptreat0)-(max(air_monthsums$SE)),max(air_monthsums$temptreat0)+(max(air_monthsums$SE))),bty="l", main="Mean Air Temp")
for (i in 1:12){
  arrows(as.numeric(air_monthsums$month[i]),air_monthsums$temptreat0[i]-air_monthsums$SE[i],as.numeric(air_monthsums$month[i]),air_monthsums$temptreat0[i]+air_monthsums$SE[i],length=0.05,angle=90,code=3)}
points(as.numeric(air_monthsums$month),air_monthsums$temptreat0,pch=21,bg="blue")
mtext("Month",side=1, line=2, adj=.5)
####Now, min soil and air temp
monthsums_allyear_min<-c()
for (i in 1:length(months)){
  monthdat<-expclim_cont[expclim_cont$month==months[i],]
  monthdat$temptreat <- relevel(as.factor( monthdat$temptreat), ref = "ambient")
  soilmod<-lmer(soiltemp1_min~temptreat + (temptreat|site), data= monthdat, REML=FALSE)
  airmod<-lmer(airtemp_min~temptreat + (temptreat|site), data=monthdat, REML=FALSE)
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
}
###Plot model results:
soil_monthsums<-monthsums_allyear_min[substring(rownames(monthsums_allyear_min),1,10)=="soil_fixed",]
air_monthsums<-monthsums_allyear_min[substring(rownames(monthsums_allyear_min),1,9)=="air_fixed",]

quartz(height=6,width=8)
par(mfcol=c(2,2),mai=c(.6,.7,.2,.1), omi=c(.7,.01,.2,.2))
#air
plot(as.numeric(air_monthsums$month),air_monthsums$temptreat0,type="p", pch=21,bg="blue", xlab="", ylab="", ylim=c(min(air_monthsums$temptreat0)-(max(air_monthsums$SE)),max(air_monthsums$temptreat0)+(max(air_monthsums$SE))),bty="l", main="Min Air Temp")
for (i in 1:12){
  arrows(as.numeric(air_monthsums$month[i]),air_monthsums$temptreat0[i]-air_monthsums$SE[i],as.numeric(air_monthsums$month[i]),air_monthsums$temptreat0[i]+air_monthsums$SE[i],length=0.05,angle=90,code=3)}
points(as.numeric(air_monthsums$month),air_monthsums$temptreat0,pch=21,bg="blue")
mtext("Difference between sham and ambient",side=2, line=2, adj=1.8)

#soil
plot(as.numeric(soil_monthsums$month),soil_monthsums$temptreat0,type="p", pch=21,bg="brown4", xlab="", ylab="", ylim=c(min(soil_monthsums$temptreat0)-(max(soil_monthsums$SE)),max(soil_monthsums$temptreat0)+(max(soil_monthsums$SE))), bty="l", main="Min Soil Temp")
for (i in 1:12){
  arrows(as.numeric(soil_monthsums$month[i]),soil_monthsums$temptreat0[i]-soil_monthsums$SE[i],as.numeric(soil_monthsums$month[i]),soil_monthsums$temptreat0[i]+soil_monthsums$SE[i],length=0.05,angle=90,code=3)}
points(as.numeric(soil_monthsums$month),soil_monthsums$temptreat0,pch=21,bg="brown4")
mtext("Month",side=1, line=2, adj=.5)

##Now max soil and air temp
monthsums_allyear_max<-c()
for (i in 1:length(months)){
  monthdat<-expclim_cont[expclim_cont$month==months[i],]
  monthdat$temptreat <- relevel(as.factor( monthdat$temptreat), ref = "ambient")
  soilmod<-lmer(soiltemp1_max~temptreat + (temptreat|site), data= monthdat, REML=FALSE)
  airmod<-lmer(airtemp_max~temptreat + (temptreat|site), data=monthdat, REML=FALSE)
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
}
###Plot model results:
soil_monthsums<-monthsums_allyear_max[substring(rownames(monthsums_allyear_max),1,10)=="soil_fixed",]
air_monthsums<-monthsums_allyear_max[substring(rownames(monthsums_allyear_max),1,9)=="air_fixed",]
#air
plot(as.numeric(air_monthsums$month),air_monthsums$temptreat0,type="p", pch=21,bg="blue", xlab="", ylab="", ylim=c(min(air_monthsums$temptreat0)-(max(air_monthsums$SE)),max(air_monthsums$temptreat0)+(max(air_monthsums$SE))),bty="l", main="Max Air Temp")
for (i in 1:12){
  arrows(as.numeric(air_monthsums$month[i]),air_monthsums$temptreat0[i]-air_monthsums$SE[i],as.numeric(air_monthsums$month[i]),air_monthsums$temptreat0[i]+air_monthsums$SE[i],length=0.05,angle=90,code=3)}
points(as.numeric(air_monthsums$month),air_monthsums$temptreat0,pch=21,bg="blue")

#soil
plot(as.numeric(soil_monthsums$month),soil_monthsums$temptreat0,type="p", pch=21,bg="brown4", xlab="", ylab="", ylim=c(min(soil_monthsums$temptreat0)-(max(soil_monthsums$SE)),max(soil_monthsums$temptreat0)+(max(soil_monthsums$SE))), bty="l", main="Max Soil Temp")
for (i in 1:12){
  arrows(as.numeric(soil_monthsums$month[i]),soil_monthsums$temptreat0[i]-soil_monthsums$SE[i],as.numeric(soil_monthsums$month[i]),soil_monthsums$temptreat0[i]+soil_monthsums$SE[i],length=0.05,angle=90,code=3)}
points(as.numeric(soil_monthsums$month),soil_monthsums$temptreat0,pch=21,bg="brown4")
mtext("Month",side=1, line=2, adj=.5)
write.csv(monthsums_allyear,"shamVSambient_meandifs.csv",row.names=FALSE)
write.csv(monthsums_allyear_max,"shamVSambient_maxdifs.csv",row.names=FALSE)
write.csv(monthsums_allyear_min,"shamVSambient_mindifs.csv",row.names=FALSE)
####Look at soil moisture now:
monthsums_allyear_mois<-c()
for (i in 1:length(months)){
  monthdat<-expclim_cont[expclim_cont$month==months[i],]
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
  }
###Plot model results:
mois_monthsums<-monthsums_allyear_mois[substring(rownames(monthsums_allyear_mois),1,10)=="mois_fixed",]
#air
plot(as.numeric(mois_monthsums$month),mois_monthsums$temptreat0,type="p", pch=21,bg="blue", xlab="Month", ylab="Difference between sham and ambient", ylim=c(min(mois_monthsums$temptreat0)-(max(mois_monthsums$SE)),max(mois_monthsums$temptreat0)+(max(mois_monthsums$SE))),bty="l", main="Soil Moisture")
for (i in 1:12){
  arrows(as.numeric(mois_monthsums$month[i]),mois_monthsums$temptreat0[i]-mois_monthsums$SE[i],as.numeric(mois_monthsums$month[i]),mois_monthsums$temptreat0[i]+mois_monthsums$SE[i],length=0.05,angle=90,code=3)}
points(as.numeric(mois_monthsums$month),mois_monthsums$temptreat0,pch=21,bg="blue")
