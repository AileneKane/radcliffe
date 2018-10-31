#Started Sept 2018
#By Ailene
#comparing data in control plots versus treatment plots versus all plots
#aim is to figure out if there is a nonlinearity in sm: interaction  response
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(lme4)
# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/radcliffe")}

#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)

#standard data wrangling to get expclim2 for climate analyses and expgdd for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")

#summarize climate data by plot (annual and seasonal temp, soil mois), 
#merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
#source("Analyses/soilmoisture/climsum_byplot_soiltoo.R")#doesn't work for some reason....
source("Analyses/soilmoisture/climsum_byplot.R")#doesn't work for some reason....
#only measured warming in control plots
#Prep the data for Stan model
expgdd_subs$sp.name<-expgdd_subs$genus.species
expgdd_subs$genus.species<-as.numeric(as.factor(expgdd_subs$genus.species))
expgdd_subs$site<-as.numeric(as.factor(expgdd_subs$site))
expgdd_subs$year<-as.numeric(as.factor(expgdd_subs$year))


#1) Divide by phenophase:

expgdd_bbd<-expgdd_subs[which(expgdd_subs$event=="bbd"),]#bud burst data
expgdd_bbd <- expgdd_bbd[apply(expgdd_bbd, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
expgdd_bbd_cont<-expgdd_bbd[expgdd_bbd$target==0,]
expgdd_bbd_treat<-expgdd_bbd[expgdd_bbd$target!=0,]
contsp<-sort(unique(expgdd_bbd_cont$genus.species))
treatsp<-sort(unique(expgdd_bbd_treat$genus.species))
expgdd_bbdmatchcsp<-expgdd_bbd[expgdd_bbd$genus.species %in% contsp,]
#make sure species are consistent across controls and treats
expgdd_bbd_cont<-expgdd_bbd_cont[expgdd_bbd_cont$genus.species %in% treatsp,]
expgdd_bbd_treat<-expgdd_bbd_treat[expgdd_bbd_treat$genus.species %in% contsp,]
expgdd_bbd_hitreat<-expgdd_bbd_treat[expgdd_bbd_treat$target>2.5,]
hitreatsp<-sort(unique(expgdd_bbd_hitreat$genus.species))
expgdd_bbd_cont2<-expgdd_bbd_cont[expgdd_bbd_cont$genus.species %in% hitreatsp,]
expgdd_bbd_treat2<-expgdd_bbd_treat[expgdd_bbd_treat$genus.species %in% hitreatsp,]
expclim_cont<-expclim2[expclim2$target==0,]
expclim_treat<-expclim2[expclim2$target!=0,]

quartz()
plot(expclim_treat$soilmois1,expclim_treat$agtemp_min,pch=21, bg="red", xlim=c(0.0,1), ylim=c(-30,40), xlab="vwc",ylab="avg min temp (jan-mar)")
plot(expclim_cont$soilmois1,expclim_cont$agtemp_min,pch=21, bg="gray")
max(expgdd_bbd$ag_min_janmar)
#Fit models

#try looking at effects of soil moisture in subsets of the data
#(high temp versus low temp versus contol)
expgdd_bbd_treat<-expgdd_bbd[expgdd_bbd$target!=0,]

dim(expgdd_bbd_cont)#6796 with matching species (6818 rows without)
dim(expgdd_bbd)#12549 rows
dim(expgdd_bbd_treat)#5722 rows with matching spp (5731 rows)
dim(expgdd_bbd_cont2)#6796 with matching species (6818 rows without)
dim(expgdd_bbd_hitreat)#5196 rows
dim(expgdd_bbd_treat2)#5722 rows with matching spp (5731 rows)

testm5cont.lmer<-lmer(doy~ag_min_janmar *soilmois_janmar  +
                        (ag_min_janmar *soilmois_janmar|genus.species)+ (1|site/year),
                      data=expgdd_bbd_cont)

testm5treat.lmer<-lmer(doy~ag_min_janmar *soilmois_janmar  +
                         (ag_min_janmar *soilmois_janmar|genus.species)+ (1|site/year),
                       data=expgdd_bbd_treat)#does not converge
testm5all.lmer<-lmer(doy~ag_min_janmar *soilmois_janmar  +
                       (ag_min_janmar *soilmois_janmar|genus.species)+ (1|site/year),
                     data=expgdd_bbd)#does not converge
testm5hitreat.lmer<-lmer(doy~ag_min_janmar *soilmois_janmar  +
                         (ag_min_janmar *soilmois_janmar|genus.species)+ (1|site/year),
                       data=expgdd_bbd_hitreat)#does not converge

#compare model coefs
cbind(round(fixef(testm5all.lmer), digits=2),round(fixef(testm5cont.lmer),digits=2),round(fixef(testm5treat.lmer), digits=2),round(fixef(testm5hitreat.lmer), digits=2))
Anova(testm5hitreat.lmer)

#Compare controls to all data

quartz(height=5,width=7)
par(mfrow=c(1,2))
hist(expgdd_bbd$soilmois_janmar, main="All plots")
mn.soilmois_janmar<-round(mean(expgdd_bbd$soilmois_janmar), digits=4)
md.soilmois_janmar<-round(median(expgdd_bbd$soilmois_janmar), digits=4)

var(expgdd_bbd$soilmois_janmar)
mtext(paste(c(mn.soilmois_janmar)))

hist(expgdd_bbd_cont$soilmois_janmar, main = "Control plots")
mn.soilmois_janmar.cont<-round(mean(expgdd_bbd_cont$soilmois_janmar), digits=4)
md.soilmois_janmar.cont<-round(median(expgdd_bbd_cont$soilmois_janmar), digits=4)

mtext(paste(mn.soilmois_janmar.cont))

cor(expgdd_bbd$ag_min_janmar,expgdd_bbd$soilmois_janmar)

#Now look ata ag_min_janmar (temperature)
quartz(height=5,width=7)
par(mfrow=c(1,2))
hist(expgdd_bbd$ag_min_janmar, main="All plots")
mn.ag_min_janmar<-round(mean(expgdd_bbd$ag_min_janmar), digits=4)
md.ag_min_janmar<-round(median(expgdd_bbd$ag_min_janmar), digits=4)

var(expgdd_bbd$ag_min_janmar)
mtext(paste(c(mn.ag_min_janmar)))

hist(expgdd_bbd_cont$ag_min_janmar, main = "Control plots")
mn.ag_min_janmar.cont<-round(mean(expgdd_bbd_cont$ag_min_janmar), digits=4)
md.ag_min_janmar.cont<-round(median(expgdd_bbd_cont$ag_min_janmar), digits=4)

mtext(paste(mn.ag_min_janmar.cont))

#look at bbdoy

#Now look ata ag_min_janmarerature
quartz(height=5,width=7)
par(mfrow=c(1,2))
hist(expgdd_bbd$doy, main="All plots")
mn.doy<-round(mean(expgdd_bbd$doy), digits=4)
md.doy<-round(median(expgdd_bbd$doy), digits=4)

var(expgdd_bbd$doy)
mtext(paste(c(mn.doy)))

hist(expgdd_bbd_cont$doy, main = "Control plots")
mn.doy.cont<-round(mean(expgdd_bbd_cont$doy), digits=4)
md.doy.cont<-round(median(expgdd_bbd_cont$doy), digits=4)

mtext(paste(mn.doy.cont))

#Now plot doy~ ag_min_janmar for controls and all
quartz(height=5,width=7)
par(mfrow=c(1,2))
plot(expgdd_bbd$ag_min_janmar,expgdd_bbd$doy, main="All plots",xlab="ag_min_janmar",ylab="BB doy")
abline(a=fixef(testm5.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
plot(expgdd_bbd_cont$ag_min_janmar,expgdd_bbd_cont$doy, main="Control plots",xlab="ag_min_janmar",ylab="BB doy")
abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5.lmer)[2], col="red")

quartz(height=5,width=7)
par(mfrow=c(1,2))
plot(expgdd_bbd$soilmois_janmar,expgdd_bbd$doy, main="All plots",xlab="vwc",ylab="BB doy")
abline(a=fixef(testm5.lmer)[1],b=fixef(testm5.lmer)[3], col="red")
plot(expgdd_bbd_cont$soilmois_janmar,expgdd_bbd_cont$doy, main="Control plots",xlab="vwc",ylab="BB doy")
abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5.lmer)[3], col="red")

#Now plot doy~ ag_min_janmar for controls and all
quartz(height=5,width=7)
par(mfrow=c(1,2))
plot(expgdd_bbd$ag_min_janmar,expgdd_bbd$doy, main="All plots",xlab="ag_min_janmar",ylab="BB doy")
abline(a=fixef(testm5.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
plot(expgdd_bbd_cont$ag_min_janmar,expgdd_bbd_cont$doy, main="Control plots",xlab="ag_min_janmar",ylab="BB doy")
abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
#look at correlation between soilmois_janmar and ag_min_janmar
quartz(height=5,width=7)
par(mfrow=c(1,2))
plot(expgdd_bbd$ag_min_janmar,expgdd_bbd$soilmois_janmar, main="All plots, r=0.32",xlab="ag_min_janmar",ylab="vwc")
abline(lm(expgdd_bbd$soilmois_janmar~expgdd_bbd$ag_min_janmar), col="red")
cor(expgdd_bbd$soilmois_janmar,expgdd_bbd$ag_min_janmar)#0.32
plot(expgdd_bbd_cont$ag_min_janmar,expgdd_bbd_cont$soilmois_janmar, main="Control plots, r=0.37",xlab="ag_min_janmar",ylab="vwc")
abline(lm(expgdd_bbd_cont$soilmois_janmar~expgdd_bbd_cont$ag_min_janmar), col="red")
cor(expgdd_bbd_cont$soilmois_janmar,expgdd_bbd_cont$ag_min_janmar)#0.37

#Compare controls to treatment
#Make plot of range of soil soilmois_janmarture in controls and in warmed
quartz(height=5,width=7)
par(mfrow=c(1,2))
hist(expgdd_bbd_treat$soilmois_janmar, main="Warmed plots")
mn.soilmois_janmar<-round(mean(expgdd_bbd_treat$soilmois_janmar), digits=4)
md.soilmois_janmar<-round(median(expgdd_bbd_treat$soilmois_janmar), digits=4)

var(expgdd_bbd_treat$soilmois_janmar)
mtext(paste(c(mn.soilmois_janmar)))

hist(expgdd_bbd_cont$soilmois_janmar, main = "Control plots")
mn.soilmois_janmar.cont<-round(mean(expgdd_bbd_cont$soilmois_janmar), digits=4)
md.soilmois_janmar.cont<-round(median(expgdd_bbd_cont$soilmois_janmar), digits=4)

mtext(paste(mn.soilmois_janmar.cont))

cor(expgdd_bbd_treat$ag_min_janmar,expgdd_bbd_treat$soilmois_janmar)

#Now look ata ag_min_janmarerature
quartz(height=5,width=7)
par(mfrow=c(1,2))
hist(expgdd_bbd_treat$ag_min_janmar, main="Warmed plots")
mn.ag_min_janmar<-round(mean(expgdd_bbd_treat$ag_min_janmar), digits=4)
md.ag_min_janmar<-round(median(expgdd_bbd_treat$ag_min_janmar), digits=4)

var(expgdd_bbd_treat$ag_min_janmar)
mtext(paste(c(mn.ag_min_janmar)))

hist(expgdd_bbd_cont$ag_min_janmar, main = "Control plots")
mn.ag_min_janmar.cont<-round(mean(expgdd_bbd_cont$ag_min_janmar), digits=4)
md.ag_min_janmar.cont<-round(median(expgdd_bbd_cont$ag_min_janmar), digits=4)

mtext(paste(mn.ag_min_janmar.cont))

#look at bbdoy

#Now look at ag_min_janmar (temperature)
quartz(height=5,width=7)
par(mfrow=c(1,2))
hist(expgdd_bbd_treat$doy, main="Warmed plots")
mn.doy<-round(mean(expgdd_bbd_treat$doy), digits=4)
md.doy<-round(median(expgdd_bbd_treat$doy), digits=4)

var(expgdd_bbd_treat$doy)
mtext(paste(c(mn.doy)))

hist(expgdd_bbd_cont$doy, main = "Control plots")
mn.doy.cont<-round(mean(expgdd_bbd_cont$doy), digits=4)
md.doy.cont<-round(median(expgdd_bbd_cont$doy), digits=4)

mtext(paste(mn.doy.cont))

#Now plot doy~ ag_min_janmar for controls and treats
quartz(height=5,width=7)
par(mfrow=c(1,2))
plot(expgdd_bbd_treat$ag_min_janmar,expgdd_bbd_treat$doy, main="Warmed plots",xlab="ag_min_janmar",ylab="BB doy")
abline(a=fixef(testm5treat.lmer)[1],b=fixef(testm5treat.lmer)[2], col="red")
plot(expgdd_bbd_cont$ag_min_janmar,expgdd_bbd_cont$doy, main="Control plots",xlab="ag_min_janmar",ylab="BB doy")
abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5cont.lmer)[2], col="red")

quartz(height=5,width=7)
par(mfrow=c(1,2))
plot(expgdd_bbd_treat$soilmois_janmar,expgdd_bbd_treat$doy, main="Warmed plots",xlab="vwc",ylab="BB doy")
abline(a=fixef(testm5treat.lmer)[1],b=fixef(testm5treat.lmer)[3], col="red")
plot(expgdd_bbd_cont$soilmois_janmar,expgdd_bbd_cont$doy, main="Control plots",xlab="vwc",ylab="BB doy")
abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5cont.lmer)[3], col="red")

#Now plot doy~ ag_min_janmar for controls and all
quartz(height=5,width=7)
par(mfrow=c(1,2))
plot(expgdd_bbd_treat$ag_min_janmar,expgdd_bbd_treat$doy, main="Warmed plots",xlab="ag_min_janmar",ylab="BB doy")
abline(a=fixef(testm5treat.lmer)[1],b=fixef(testm5treat.lmer)[2], col="red")
plot(expgdd_bbd_cont$ag_min_janmar,expgdd_bbd_cont$doy, main="Control plots",xlab="ag_min_janmar",ylab="BB doy")
abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5cont.lmer)[2], col="red")
#look at correlation between soilmois_janmar and ag_min_janmar
quartz(height=5,width=7)
par(mfrow=c(1,2))
plot(expgdd_bbd_treat$ag_min_janmar,expgdd_bbd_treat$soilmois_janmar, main="Warmed plots, r=0.32",xlab="ag_min_janmar",ylab="vwc")
abline(lm(expgdd_bbd_treat$soilmois_janmar~expgdd_bbd_treat$ag_min_janmar), col="red")
cor(expgdd_bbd_treat$soilmois_janmar,expgdd_bbd_treat$ag_min_janmar)#0.32
plot(expgdd_bbd_cont$ag_min_janmar,expgdd_bbd_cont$soilmois_janmar, main="Control plots, r=0.37",xlab="ag_min_janmar",ylab="vwc")
abline(lm(expgdd_bbd_cont$soilmois_janmar~expgdd_bbd_cont$ag_min_janmar), col="red")
cor(expgdd_bbd_cont$soilmois_janmar,expgdd_bbd_cont$ag_min_janmar)#0.37



#Compare high vs low temps (regardless of treatment)
