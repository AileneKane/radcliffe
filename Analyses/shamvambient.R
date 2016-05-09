###Analyses of sham ("control") vs. "ambient" microclimate data, to examine how warming chambers may alter micrclimate.
#Look at temperature, soil moisture
setwd("~/GitHub/radcliffe/Analyses")
expclim<-read.csv("gddchill/expclim.wchillgdd.csv", header=T)
head(expclim)
unique(expclim$temptreat)
##want to plot "0" compared to "ambient" in each site
##select out just these two treatments for now
expclim_controls<-expclim[expclim$temptreat=="0" | expclim$temptreat=="ambient", ]
unique(expclim_controls$site)
sitesums<-data.frame(tapply(expclim_controls$soiltemp1_mean,list(expclim_controls$site,expclim_controls$temptreat),length))
colnames(sitesums)<-c("sham.control","ambient")
sites_con<-rownames(sitesums)[!is.na(sitesums$sham.control) & !is.na(sitesums$ambient)]
sites_con<-sites_con[-which(sites_con=="chuine")]
expclim_cont<-expclim_controls[expclim_controls$site %in% sites_con,]
#make figures of sham controls vs. ambient conditions:
#mean soil temp:
quartz(height=4, width=8)
par(mfrow=c(1,5))
for (i in 1:length(sites_con)){
  dat<-expclim_cont[expclim_cont$site==sites_con[i],]
  if(length(unique(dat$soiltemp1_mean))==1){next}
  boxplot(dat$soiltemp1_mean~dat$temptreat, main=paste(sites_con[i]), ylab="Mean soil temp")
}
mod2<-lmer(soiltemp1_mean~-1+temptreat + (1|site), data=expclim_cont, REML=FALSE)
coefs2<-data.frame(coef(summary(mod2)))
#plot.new()
#mtext(coefs)
#looks like soil temperatures are colder in the shams
####Now mean air temp (for the 4 sites that have this)
quartz(height=4, width=8)
par(mfrow=c(1,4))
for (i in 1:length(sites_con)){
  dat<-expclim_cont[expclim_cont$site==sites_con[i],]
  if(length(unique(dat$airtemp_mean))==1){next}
  boxplot(dat$airtemp_mean~dat$temptreat, main=paste(sites_con[i]), ylab="Mean air temp")
}
#looks like air temperatures are WARMER in the shams
expclim_cont$temptreat <- relevel(as.factor(expclim_cont$temptreat), ref = "ambient")
mod<-lmer(airtemp_mean~-1+temptreat + (1|site), data=expclim_cont, REML=FALSE)
coefs<-data.frame(coef(summary(mod)))
print(coef(summary(mod)))
#Do we see the same patters for max and min values? OR are these altered in different ways 
mod_max<-lmer(airtemp_max~temptreat + (1|site), data=expclim_cont, REML=FALSE)
coefs_max<-data.frame(coef(summary(mod_max)))
mod_min<-lmer(airtemp_min~temptreat + (1|site), data=expclim_cont, REML=FALSE)
coefs_min<-data.frame(coef(summary(mod_min)))
coefs_max
coefs_min

mod2_max<-lmer(soiltemp1_max~temptreat + (1|site), data=expclim_cont, REML=FALSE)
coefs2_max<-data.frame(coef(summary(mod2_max)))
mod2_min<-lmer(soiltemp1_min~temptreat + (1|site), data=expclim_cont, REML=FALSE)
coefs2_min<-data.frame(coef(summary(mod2_min)))
coefs2_max
coefs2_min
co