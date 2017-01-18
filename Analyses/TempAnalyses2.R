#Additional analyses for Experimental Climate Paper
#Analysis of Mean, Min & Max Temperature (and variance) compared to target and reported
#By Ailene Ettinger
#Started 17 January 2017
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
#want to compare mean and variances of min and max temperatures in control plots and warmed plots in each study
#using two types structural controls separately
expclim2<-full_join(treats,expclim, by=c("site", "block", "plot","temptreat","preciptreat"), match="first")
#select only rows that do not manipulated precipitation
expclimt<-expclim2[which(expclim2$preciptreat==0|is.na(expclim2$preciptreat)),]
#get one column for above-ground temperature
expclimt$agtemp_min<-expclimt$airtemp_min
expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$cantemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$airtemp_min) & !is.na(expclimt$cantemp_min)),]$cantemp_min
expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$surftemp_min
expclimt$agtemp_max<-expclimt$airtemp_max
expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$cantemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$airtemp_max) & !is.na(expclimt$cantemp_max)),]$cantemp_max
expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$surftemp_max

####Try coefficient of variation for each plot during each year and treatment and temperature type
#convert all temepratures to Kelvin to avoid negative values (CV doesn't work when there is a mix of positive and negative values
expclimt$agtemp_max_k<-expclimt$agtemp_max+273.15
expclimt$agtemp_min_k<-expclimt$agtemp_min+273.15
expclimt$soiltemp1_min_k<-expclimt$soiltemp1_min+273.15
expclimt$soiltemp1_max_k<-expclimt$soiltemp1_max+273.15
expclimt$date<-strptime(strptime(paste(expclimt$year,expclimt$doy,sep="-"), format = "%Y-%j"),format = "%Y-%m-%d")
expclimt$month<-substr(expclimt$date,6,7)

cv_agtemp_max<-aggregate(expclimt$agtemp_max_k, by=list(expclimt$site,expclimt$plot,expclimt$year,expclimt$month,expclimt$temptreat), FUN=cv,na.rm=TRUE)
colnames(cv_agtemp_max)<-c("site","plot","year","month","temptreat","agtemp_max")
cv_agtemp_max <- cv_agtemp_max[order(cv_agtemp_max$site,cv_agtemp_max$plot,cv_agtemp_max$year,cv_agtemp_max$month),] 
#now Min AG temp
cv_agtemp_min<-aggregate(expclimt$agtemp_min_k, by=list(expclimt$site,expclimt$plot,expclimt$year,expclimt$month,expclimt$temptreat), FUN=cv,na.rm=TRUE)
colnames(cv_agtemp_min)<-c("site","plot","year","month","temptreat","agtemp_min")
cv_agtemp_min <- cv_agtemp_min[order(cv_agtemp_min$site,cv_agtemp_min$plot,cv_agtemp_min$year,cv_agtemp_min$month),] 

#BG Max Temp
cv_bgtemp_max<-aggregate(expclimt$soiltemp1_max_k, by=list(expclimt$site,expclimt$plot,expclimt$year,expclimt$month,expclimt$temptreat), FUN=cv,na.rm=TRUE)
colnames(cv_bgtemp_max)<-c("site","plot","year","month","temptreat","bgtemp_max")
cv_bgtemp_max <- cv_bgtemp_max[order(cv_bgtemp_max$site,cv_bgtemp_max$plot,cv_bgtemp_max$year,cv_bgtemp_max$month),] 

#BG Min Temp
cv_bgtemp_min<-aggregate(expclimt$soiltemp1_min_k, by=list(expclimt$site,expclimt$plot,expclimt$year,expclimt$month,expclimt$temptreat), FUN=cv,na.rm=TRUE)
colnames(cv_bgtemp_min)<-c("site","plot","year","month","temptreat","bgtemp_min")
cv_bgtemp_min <- cv_bgtemp_min[order(cv_bgtemp_min$site,cv_bgtemp_min$plot,cv_bgtemp_min$year,cv_bgtemp_min$month),] 

#Now combine the four temperature variables
dim(cv_bgtemp_min);dim(cv_bgtemp_max);dim(cv_agtemp_min);dim(cv_agtemp_max)
#Add new column for temptreat that can be merged with the files so that it has target warming instead of level
cv_all<-cbind(cv_agtemp_max,cv_agtemp_min[,6],cv_bgtemp_max[,6],cv_bgtemp_min[,6])
colnames(cv_all)[6:9]<-c("cv_agtemp_max","cv_agtemp_min","cv_bgtemp_max","cv_bgtemp_min")
#colnames(cv_all)[4]<-"temptreatx"
#cv_all$temptreat<-NA
#cv_all[which(cv_all$temptreatx=="ambient"),]$temptreat<-cv_all[which(cv_all$temptreatx=="ambient"),]$temptreatx
#now merge target temperatures in
treats2<-subset(treats,select=c("site","plot","temptreat","target","reported"))
treats2 <- treats %>% # start with the data frame
  distinct(site, plot,temptreat,.keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(site,plot,temptreat,target,reported)
cv_allt <- right_join(treats2,cv_all, by=c("site","plot","temptreat"), match="all")
cv_allt[which(cv_allt$temptreat=="ambient"),]$target<--1
cv_allt[which(cv_allt$temptreat=="0"),]$target<-0

unique(cv_allt$temptreat)
#now figure
#remake figure with target temp on x axis, and color coding by target warming
#targetcol<-c("black","gray","white","#FFF5F0","#FEE0D2","#FCBBA1","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
quartz(height=5,width=10)
par(mfrow=c(2,2),mai=c(.3,.6,.2,.05),omi=c(.5,.5,.2,.5))
plot(as.numeric(cv_allt$target),cv_allt$cv_agtemp_max,xlab="",xaxt="n",ylab="",ylim=c(0,4), bty="l", cex.axis=.9, main="Max AG Temp CV",pch = 21)
mtext(side=2,"Coefficient of Variation (K)", line=2,adj=10,cex=.9)
axis(side=1,at=c(-1,0,1,2,3,4,5,6,7), labels=FALSE,cex=.9)
plot(as.numeric(cv_allt$target),cv_allt$cv_agtemp_min,xlab="",xaxt="n",ylab="",ylim=c(0,4), bty="l", cex.axis=.9, main="Min AG temp",pch = 21)
axis(side=1,at=c(-1,0,1,2,3,4,5,6,7), labels=FALSE,cex=.9)
mtext(side=2,"Coefficient of Variation (K)", line=2,adj=10,cex=.9)

plot(as.numeric(cv_allt$target),cv_allt$cv_bgtemp_max,xlab="",xaxt="n",ylab="",ylim=c(0,4), bty="l", cex.axis=.9, main="Max BG temp",pch = 21)
axis(side=1,at=c(-1,0,1,2,3,4,5,6,7), labels=FALSE,cex=.9)
mtext(side=1,"Target warming (degrees C)", line=2.3, adj=.5)
axis(side=1,at=c(-1,0,1,2,3,4,5,6,7), labels=c("amb","struct","1","2","3","4","5","6","7"),cex=.9)

plot(as.numeric(cv_allt$target),cv_allt$cv_bgtemp_min,xlab="",xaxt="n",ylab="",ylim=c(0,4), bty="l", cex.axis=.9, main="Min BG temp",pch = 21)
axis(side=1,at=c(-1,0,1,2,3,4,5,6,7), labels=c("amb","struct","1","2","3","4","5","6","7"),cex=.9)
mtext(side=1,"Target warming (degrees C", line=2.3, adj=.5)
legend(x=,y=100,legend=sort(unique(cv_allt$site)),pch=unique(as.numeric(cv_allt$site)))+20,bty="n",cex=0.7,pt.cex=0.7)

quartz(height=5,width=10)
par(mfrow=c(2,2),mai=c(.3,.6,.2,.05),omi=c(.5,.5,.2,.5))
boxplot(cv_allt$cv_agtemp_max~as.factor(cv_allt$target),ylab="",ylim=c(0,4), bty="l", cex.axis=.9, main="Max. above-ground temperature")
mtext(side=2,"Coefficient of Variation (K)", line=2,adj=10,cex=.9)
boxplot(cv_allt$cv_agtemp_min~as.factor(cv_allt$target),ylab="",ylim=c(0,4), bty="l", cex.axis=.9, main="Min. above-ground temperature")
mtext(side=2,"Coefficient of Variation (K)", line=2,adj=10,cex=.9)
boxplot(cv_allt$cv_bgtemp_max~as.factor(cv_allt$target),ylab="",xlab="Target warming (degrees C",ylim=c(0,4), bty="l", cex.axis=.9, main="Max. below-ground temperature")
mtext(side=1,"Target warming (degrees C)", line=2,adj=.5,cex=.9)
boxplot(cv_allt$cv_bgtemp_min~as.factor(cv_allt$target),ylab="",ylim=c(0,4), bty="l", cex.axis=.9, main="Min. below-ground temperature")
mtext(side=1,"Target warming (degrees C)", line=2,adj=.5,cex=.9)

#Fit models:
cv_allt$site<-as.factor(cv_allt$site)
cv_allt$year<-as.factor(cv_allt$year)
cv_allt$warm<-"warmed"#for actively warmed sites
cv_allt[which(cv_allt$temptreat=="0"),]$warm<-"struc_cont"#for structural controls
cv_allt[which(cv_allt$temptreat=="ambient"),]$warm<-"amb_cont"#for ambient controls, which will be the reference
smincv_mod<-lmer(cv_bgtemp_min~-1+warm+(1|site/year), data=cv_allt, REML=FALSE)
smincv_mod1<-lmer(cv_bgtemp_min~warm+(warm|site/year), data=cv_allt, REML=FALSE)
AIC(smincv_mod,smincv_mod1)#1 wins
summary(smincv_mod)
#all experiments have reduced variation
smaxcv_mod<-lmer(cv_bgtemp_max~-1+warm + (1|site/year), data=cv_allt, REML=FALSE)
smaxcv_mod1<-lmer(cv_bgtemp_max~warm + (warm|site/year), data=cv_allt, REML=FALSE)
AIC(smaxcv_mod,smaxcv_mod1)#mod wins
summary(smaxcv_mod)
cv_allt$warm <- relevel(as.factor( cv_allt$warm), ref = "struc_cont")

#actively warmed plots have reduced variation in soil temperatures, though it is negligible for minimum temperatures (more pronounced for soil max)
amaxcv_mod<-lmer(cv_agtemp_max~warm + (1|site/year), data=cv_allt, REML=FALSE)
amaxcv_mod1<-lmer(cv_agtemp_max~warm + (warm|site/year), data=cv_allt, REML=FALSE)
AIC(amaxcv_mod,amaxcv_mod1)#mod wins
summary(amaxcv_mod)

amincv_mod<-lmer(cv_agtemp_min~warm + (1|site/year), data=cv_allt, REML=FALSE)
#amincv_mod1<-lmer(cv_agtemp_min~warm + (warm|site/year), data=cv_allt, REML=FALSE)
#AIC(amincv_mod,amincv_mod1)#mod wins
summary(amincv_mod)
amincv_coefs<-coef(summary(amincv_mod))

amaxcv_coefs<-coef(summary(amaxcv_mod))

#actively warmed plots have increased variation in air temperatures (both min and max)
#Make a plot:
quartz(height=6,width=4)
par(mfcol=c(2,1),mai=c(.6,.7,.2,.1), omi=c(.7,.01,.2,.2))
#air, cv_agtemp_max~warm,data=cv_allt
plot(as.numeric(air_monthsums$month),air_monthsums$temptreat0,type="p", pch=21,bg="black", xlab="", ylab="", ylim=c(-2,2),bty="l", main="Min Air Temp", las=TRUE)
#add random effects
minaexp03<-minairranef[which(minairranef$site=="exp03"),]
minaexp04<-minairranef[which(minairranef$site=="exp04"),]
minaexp07<-minairranef[which(minairranef$site=="exp07"),]
minaexp10<-minairranef[which(minairranef$site=="exp10"),]
points(as.numeric(minaexp03$month),minaexp03$shamdif,pch=21,bg="lightsalmon",col="lightsalmon")
points(as.numeric(minaexp04$month),minaexp04$shamdif,pch=22,bg="lightblue",col="lightblue")
points(as.numeric(minaexp07$month),minaexp07$shamdif,pch=23,bg="lightblue",col="lightblue")
points(as.numeric(minaexp10$month),minaexp10$shamdif,pch=24,bg="lightsalmon",col="lightsalmon")
for (i in 1:12){
  arrows(as.numeric(air_monthsums$month[i]),air_monthsums$temptreat0[i]-air_monthsums$SE[i],as.numeric(air_monthsums$month[i]),air_monthsums$temptreat0[i]+air_monthsums$SE[i],length=0.01,angle=90,code=3)}
abline(h=0,lty=2)
points(as.numeric(air_monthsums$month),air_monthsums$temptreat0,pch=21,bg="black")
mtext("Effect of structural control (difference between sham & ambient, C)",side=2, line=2, adj=.99)
legend(1,-.3,legend=c("exp03","exp04","exp07","exp10"), pch=c(21,22,23,24),pt.bg=c("lightsalmon","lightblue","lightblue","lightsalmon"),col=c("lightsalmon","lightblue","lightblue","lightsalmon"),bty="n")



#Now see if warming increases min and max temp the same magnitude as mean
expclimt[which(expclimt$temptreat=="0"),]$target<-0#for structural controls
expclimt[which(expclimt$temptreat=="ambient"),]$target<-0#for ambient controls, which will be the reference
amin_mod<-lmer(agtemp_min~target + (target|site/year), data=expclimt, REML=FALSE)
summary(amin_mod)#warms, on average 0.84 degrees per target degree, for air min
amax_mod<-lmer(agtemp_max~target + (target|site/year), data=expclimt, REML=FALSE)
summary(amax_mod)#warms, on average 0.51 degrees per target degree, for air min
smin1_mod<-lmer(soiltemp1_min~target + (target|site/year), data=expclimt, REML=FALSE)
summary(smin1_mod)#warms, on average 0.68 degrees per target degree, for soil min
smax1_mod<-lmer(soiltemp1_max~target + (target|site/year), data=expclimt, REML=FALSE)
summary(smax1_mod)##warms, on average 0.67 degrees per target degree, for soil max
smean1_mod<-lmer(soiltemp1_mean~target + (target|site/year), data=expclimt, REML=FALSE)
summary(smean1_mod)##warms, on average 0.70 degrees per target degree, for soil max
#compare these structures to random intercept only model:
#amin_mod1<-lmer(agtemp_min~target + (1|site/year), data=expclimt, REML=FALSE)
#summary(amin_mod1)#warms, on average 0.67 degrees per targert degree, for both min and max
#amax_mod1<-lmer(agtemp_max~target + (1|site/year), data=expclimt, REML=FALSE)
#summary(amax_mod1)
#smin1_mod1<-lmer(soiltemp1_min~target + (1|site/year), data=expclimt, REML=FALSE)
#summary(smin1_mod1)#warms, on average 0.70 degrees per target degree, for soil min
#smax1_mod1<-lmer(soiltemp1_max~target + (1|site/year), data=expclimt, REML=FALSE)
#summary(smax1_mod1)##warms, on average 0.73 degrees per target degree, for soil max
#AIC(smax1_mod1,smax1_mod)#lower for random slopes in all cases
#AIC(smin1_mod1,smin1_mod)
#AIC(amax_mod1,amax_mod)
#AIC(amin_mod1,amin_mod)
#Now try temperature range:
expclimt$airtemp_range<-expclimt$agtemp_max-expclimt$agtemp_min
atemprange_mod<-lmer(airtemp_range~target + (target|site/year), data=expclimt, REML=FALSE)
atemprange_mod1<-lmer(airtemp_range~target + (1|site/year), data=expclimt, REML=FALSE)
AIC(atemprange_mod,atemprange_mod1)
summary(atemprange_mod)##target temperature has a negative effect on
expclimt$warm<-"warmed"#for actively warmed sites
expclimt[which(expclimt$temptreat=="0"),]$warm<-"struc_cont"#for structural controls
expclimt[which(expclimt$temptreat=="ambient"),]$warm<-"amb_cont"#for ambient controls, which will be the reference
expclimt$warm <- relevel(as.factor( expclimt$warm), ref = "struc_cont")
#actively warmed plots have reduced variation in soil temperatures, though it is negligible for minimum temperatures (more pronounced for soil max)
arange_mod<-lmer(airtemp_range~warm + (1|site/year), data=expclimt, REML=FALSE)
arange_mod1<-lmer(airtemp_range~warm + (warm|site/year), data=expclimt, REML=FALSE)
AIC(arange_mod,arange_mod1)#mod wins
summary(arange_mod1)
boxplot(airtemp_range~warm, data=expclimt)
boxplot(cv_agtemp_min~warm, data=cv_allt)
## Now try looking at each month to see if the variation has seasonal patterns
months<-sort(unique(cv_allt$month))

soilcv_all<-data.frame(matrix(NA, nrow = 36, ncol = 7))
aircv_all<-data.frame(matrix(NA, nrow = 36, ncol = 7))
for (i in 1:length(months)){
  monthdat<-cv_allt[cv_allt$month==months[i],]
  #monthdat$temptreat <- relevel(as.factor( monthdat$temptreat), ref = "ambient")
  smaxcv_mod<-lmer(cv_bgtemp_max~-1+warm + (1|site/year), data=monthdat, REML=FALSE)
  smincv_mod<-lmer(cv_bgtemp_min~-1+warm + (1|site/year), data=monthdat, REML=FALSE)
  amaxcv_mod<-lmer(cv_agtemp_max~-1+warm + (1|site/year), data=monthdat, REML=FALSE)
  amincv_mod<-lmer(cv_agtemp_min~-1+warm + (1|site/year), data=monthdat, REML=FALSE)
  coefs_air<-cbind(data.frame(coef(summary(amaxcv_mod))),data.frame(coef(summary(amincv_mod))))
  coefs_soil<-cbind(data.frame(coef(summary(smaxcv_mod))),data.frame(coef(summary(smincv_mod))))
  colnames(coefs_air)[1]<-"airtmax_cv";colnames(coefs_air)[4]<-"airtmin_cv"
  colnames(coefs_soil)[1]<-"soiltmax_cv";colnames(coefs_soil)[4]<-"soiltmin_cv"
  coefs_air$month<-paste(i);  coefs_soil$month<-paste(i)
  soilcv_all[((i*3)-2):(i*3),]<-coefs_soil
  aircv_all[((i*3)-2):(i*3),]<-coefs_air
}
colnames(soilcv_all)<-colnames(coefs_soil)
colnames(aircv_all)<-colnames(coefs_air)
soilcv_all$coef<-c(rep(rownames(coefs_soil),times=12))
aircv_all$coef<-c(rep(rownames(coefs_air),times=12))
rownames(soilcv_all)<-NULL
rownames(aircv_all)<-NULL
soil_warmed<-subset(soilcv_all,coef=="warmwarmed")
air_warmed<-subset(aircv_all,coef=="warmwarmed")
#
quartz(height=6,width=7)
par(mfrow=c(2,2),mai=c(.5,.7,.2,.1), omi=c(.4,.01,.2,.2))
#airmax
plot(as.numeric(air_warmed$month),air_warmed$airtmax_cv,type="p", pch=21,bg="gray", xlab="Month", ylab="", ylim=c(-.2,.2),bty="l", main="Max Air Temperature")
abline(h=0,lty=2)
for (i in 1:12){
  arrows(as.numeric(air_warmed$month[i]),air_warmed$airtmax_cv[i]-air_warmed$Std..Error[i],as.numeric(air_warmed$month[i]),air_warmed$airtmax_cv[i]+air_warmed$Std..Error[i],length=0,angle=90,code=0)}
points(as.numeric(air_warmed$month),air_warmed$airtmax_cv,pch=21,bg="gray")
#mtext("a)",side=3, line=1, adj=0)
#airmin
plot(as.numeric(air_warmed$month),air_warmed$airtmin_cv,type="p", pch=21,bg="gray", xlab="Month", ylab="", ylim=c(-.2,.2),bty="l", main="Min Air Temperature")
abline(h=0,lty=2)
for (i in 1:12){
  arrows(as.numeric(air_warmed$month[i]),air_warmed$airtmin_cv[i]-air_warmed$Std..Error.1[i],as.numeric(air_warmed$month[i]),air_warmed$airtmin_cv[i]+air_warmed$Std..Error.1[i],length=0,angle=90,code=0)}
points(as.numeric(air_warmed$month),air_warmed$airtmin_cv,pch=21,bg="gray")
#mtext("a)",side=3, line=1, adj=0)
#soilmax
plot(as.numeric(soil_warmed$month),soil_warmed$soiltmax_cv,type="p", pch=21,bg="gray", xlab="Month", ylab="", ylim=c(-.2,.2),bty="l", main="Max Soil Temperature")
abline(h=0,lty=2)
for (i in 1:12){
  arrows(as.numeric(soil_warmed$month[i]),soil_warmed$soiltmax_cv[i]-soil_warmed$Std..Error[i],as.numeric(soil_warmed$month[i]),soil_warmed$soiltmax_cv[i]+soil_warmed$Std..Error[i],length=0,angle=90,code=0)}
points(as.numeric(soil_warmed$month),soil_warmed$soiltmax_cv,pch=21,bg="gray")
#soilmin
plot(as.numeric(soil_warmed$month),soil_warmed$soiltmin_cv,type="p", pch=21,bg="gray", xlab="Month", ylab="", ylim=c(-.2,.2),bty="l", main="Min Soil Temperature")
abline(h=0,lty=2)
for (i in 1:12){
  arrows(as.numeric(soil_warmed$month[i]),soil_warmed$soiltmin_cv[i]-soil_warmed$Std..Error.1[i],as.numeric(soil_warmed$month[i]),soil_warmed$soiltmin_cv[i]+soil_warmed$Std..Error.1[i],length=0,angle=90,code=0)}
points(as.numeric(soil_warmed$month),soil_warmed$soiltmin_cv,pch=21,bg="gray")

mtext("Month",side=1, line=2, adj=.5)
#mtext("b)",side=3, line=1, adj=0)


#try plotting no-intercept model
quartz(height=6,width=7)
par(mfrow=c(2,2),mai=c(.5,.7,.2,.1), omi=c(.4,.01,.2,.2))
#airmax
plot(as.numeric(aircv_all$month)+c(0,.2,.4),aircv_all$airtmax_cv,type="p", pch=(as.numeric(as.factor(aircv_all$coef))+20),bg="gray", xlab="Month", ylab="", ylim=c(.5,2.5),bty="l", main="Max Air Temperature")
mtext("Coefficient of Variation",side=2, line=1, adj=0)
#airmin
plot(as.numeric(aircv_all$month)+c(0,.2,.4),aircv_all$airtmin_cv,type="p", pch=(as.numeric(as.factor(aircv_all$coef))+20),bg="gray", xlab="Month", ylab="", ylim=c(.5,2.5),bty="l", main="Max Air Temperature")
#soilmax
plot(as.numeric(soilcv_all$month)+c(0,.2,.4),soilcv_all$soiltmax_cv,type="p", pch=(as.numeric(as.factor(soilcv_all$coef))+20),bg="gray", xlab="Month", ylab="", ylim=c(0,1.5),bty="l", main="Max Soil Temperature")
mtext("Coefficient of Variation",side=2, line=1, adj=0)

#soilmin
plot(as.numeric(soilcv_all$month)+c(0,.2,.4),soilcv_all$soiltmin_cv,type="p", pch=(as.numeric(as.factor(soilcv_all$coef))+20),bg="gray", xlab="Month", ylab="", ylim=c(0,1.5),bty="l", main="Max Soil Temperature")
mtext("Month",side=1, line=2, adj=.5)
#mtext("b)",side=3, line=1, adj=0)
