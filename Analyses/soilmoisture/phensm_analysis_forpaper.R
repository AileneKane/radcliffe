#Started Sept 2017
#By Ailene
#Two questions to address:
#1) How do soil moisture and temperature affect doy of bud burst, leaf out, etc?
#2) How do warming and precip treatments affect soil moisture? (Make plots and fit models)

#Use brms/Stan to fit soil moisture- phenology model to radcliffe data 
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE
library(lme4)
library(rstan)
library(ggplot2)
library(shinystan)
library(bayesplot)

library(rstanarm)
library(dplyr)
library(brms)
library(RColorBrewer)
library(plotrix)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/radcliffe")}
#setwd("~/Documents/GitHub/radcliffe")#noaa
#Goal: Fit a multi-model to phenology (budburst) data with temperature, soil moisture, and 
#their interaction as explanatory variables.

source('Analyses/soilmoisture/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)

remove.conifers=TRUE
use.airtemponly=TRUE
use.centmod=FALSE

#standard data wrangling to get expclim2 for climate analyses and expgdd for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")

#summarize climate data by plot (annual and seasonal temp, soil mois), 
  #merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs

if(use.airtemponly==TRUE) {source("Analyses/source/climsum_byplot.R")}
if(use.airtemponly==FALSE) {source("Analyses/source/climsum_byplot_soiltoo.R")}

#####Phenology models#####

#Want to fit a model with soil moisture and above-ground temperature as predictors for doy of phenological event
#Start by looking at which studies have both SM and AG temp data
#which(tapply(expclim2$agtemp_mn,expclim2$site,mean,na.rm=T)>0)
#which(tapply(expclim2$soilmois1,expclim2$site,mean,na.rm=T)>0)

#Prep the data for models and divide into datasets by phenophase
source("Analyses/source/phensm_modprep.R")#should this be "stanprep_phenmods.R"?
##################
# Fit m5 to data #
##################
#
testm5cent.lmer<-lmer(y~temp * mois +  (temp*mois|sp)+ (1|site/year),
                  data=datalist.bbd.cent)
summary(testm5cent.lmer)
agtempcoefs<-fixef(testm5cent.lmer)
#compare to soil temp model
testm5centbg.lmer<-lmer(y~temp * mois +  (temp*mois|sp)+ (1|site/year),
                      data=datalist.bbdsoil.cent)
summary(testm5centbg.lmer)
bgtempcoefs<-fixef(testm5centbg.lmer)
#compare
cbind(agtempcoefs,bgtempcoefs)


testm5cent.int.lmer<-lmer(y~temp * mois + (1|sp)+ (1|site/year),data=datalist.bbd.cent, REML=FALSE)
testm5cent.intsq.lmer<-lmer(y~ mois*(temp + I(temp^2)) + (1|sp)+ (1|site/year),data=datalist.bbd.cent, REML=FALSE)
testm5cent.intsq.lmer2<-lmer(y~ mois*(temp + I(temp^2)) + (mois*(temp + I(temp^2))|sp)+ (1|site/year),data=datalist.bbd.cent, REML=FALSE)
testm5tempcent.intsq.lmer<-lmer(y~temp + temp2 + (1|sp)+ (1|site/year),data=datalist.bbd.cent)
testm5tempcent.lmer<-lmer(y~temp + (temp|sp)+ (1|site/year),data=datalist.bbd.cent)
testm5tempcent.sq.lmer<-lmer(y~temp + temp2 + (temp + temp2|sp)+ (1|site/year),data=datalist.bbd.cent)
anova(testm5cent.int.lmer,testm5cent.intsq.lmer,testm5tempcent.int.lmer,testm5tempcent.intsq.lmer,
      testm5tempcent.lmer,testm5tempcent.sq.lmer,testm5cent.intsq.lmer2)
#best model is testm5cent.intsq.lmer2 
#try to fit it in rstanarm 
bbd.cent <- with(expgdd_bbd, 
                          data.frame(y = doy, 
                               temp = ag_min_jm_cent[,1], #above-ground minimum air temp
                               temp2 = ag_min_jm_cent[,1]*ag_min_jm_cent[,1], #above-ground minimum air temp
                               mois = smjm_cent[,1], #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_bbd),
                               n_sp = length(unique(expgdd_bbd$genus.species))
                          )
)
testm5cent.intsq.rstan2<-stan_lmer(
    y~ mois*(temp + I(temp^2)) + #fixed
      (mois*(temp + I(temp^2))|sp)+ (1|site/year),#random
    data=bbd.cent)#data, etc




fe <- summary(testm5cent.intsq.lmer2)$coefficients[1:6,1]
#below doesnt' work
quartz()
plot_df <- expand.grid(temp=datalist.bbd.cent$temp, mois=c(min(datalist.bbd.cent$mois), mean(datalist.bbd.cent$mois), max(datalist.bbd.cent$mois))) %>% 
  mutate(doy = fe[1] + fe[2]*mois + fe[3]*temp + fe[4]*temp*temp + fe[5]*mois*temp + fe[6]*mois*temp*temp)
plot_interaction %+% plot_df

#look at quadratic fit:
quartz()
ggplot(expgdd_bbd, aes(ag_min_jm_cent, doy)) + geom_point() + geom_smooth(method="lm", formula=y~poly(x, 2))

summary(testm5cent.lmer)

testm5cent.lmer<-lmer(y~temp * mois +
                        (temp*mois|sp)+ (1|site/year),
                      data=datalist.bbd.cent)
summary(testm5cent.lmer)


#now try fitting quadratic model in rstanarm


#model fits- onyl when centered! a=96.4445,temp= -9.3089; mois=-1.5954; temp:mois= 0.7739
#try adding year as a fixed effect
testm5yr.lmer<-lmer(y~temp * mois * year +
                       (temp*mois |sp)+ (1|site/year),
                     data=datalist.bbd.cent)#has higher aic than model without year
summary(testm5yr.lmer)
#testm5yr2.lmer<-lmer(y~temp * mois * year +
#                    (temp*mois*year |sp)+ (1|site/year),
#                  data=datalist.bbd)#doesn't converge
AIC(testm5.lmer,testm5yr.lmer)
#look at year as a predictor

#testm5cont.lmer<-lmer(y~temp * mois +
#                    (temp*mois|sp)+ (1|site/year),
#                  data=datalist.bbdcont)
#summary(testm5cont.lmer)
testm5contcent.lmer<-lmer(y~temp * mois +
                        (temp*mois|sp)+ (1|site/year),
                      data=datalist.bbdcont.cent)
summary(testm5contcent.lmer)

#compare coefs from full dataset and control dataset
comp.coefs.lmer<-cbind(round(fixef(testm5cent.lmer), digits=3),round(fixef(testm5contcent.lmer), digits=3))
colnames(comp.coefs.lmer)<-c("m5.cent","m5cont.cent")
write.csv(comp.coefs.lmer,"Analyses/soilmoisture/comp.coefs.lmer.csv", row.names = TRUE)
Anova(testm5contcent.lmer)
Anova(testm5cent.lmer)

#compare coefs in with soil temp versus air temp
colnames(expgdd)
#mois not significant in controls; is marginally significant in full dataset
#Make plot of range of soil moisture in controls and in all
quartz(height=5,width=7)
par(mfrow=c(1,2))
hist(datalist.bbd$mois, main="All plots")
mn.mois<-round(mean(datalist.bbd$mois), digits=4)
md.mois<-round(median(datalist.bbd$mois), digits=4)

var(datalist.bbd$mois)
mtext(paste(c(mn.mois)))

hist(datalist.bbdcont$mois, main = "Control plots")
mn.mois.cont<-round(mean(datalist.bbdcont$mois), digits=4)
md.mois.cont<-round(median(datalist.bbdcont$mois), digits=4)

mtext(paste(mn.mois.cont))

cor(datalist.bbd$temp,datalist.bbd$mois)

#Now look ata temperature
quartz(height=5,width=7)
par(mfrow=c(1,2))
hist(datalist.bbd$temp, main="All plots")
mn.temp<-round(mean(datalist.bbd$temp), digits=4)
md.temp<-round(median(datalist.bbd$temp), digits=4)

var(datalist.bbd$temp)
mtext(paste(c(mn.temp)))

hist(datalist.bbdcont$temp, main = "Control plots")
mn.temp.cont<-round(mean(datalist.bbdcont$temp), digits=4)
md.temp.cont<-round(median(datalist.bbdcont$temp), digits=4)

mtext(paste(mn.temp.cont))

#look at bbdoy

#Now look ata temperature
quartz(height=5,width=7)
par(mfrow=c(1,2))
hist(datalist.bbd$y, main="All plots")
mn.doy<-round(mean(datalist.bbd$y), digits=4)
md.doy<-round(median(datalist.bbd$y), digits=4)

var(datalist.bbd$y)
mtext(paste(c(mn.doy)))

hist(datalist.bbdcont$y, main = "Control plots")
mn.doy.cont<-round(mean(datalist.bbdcont$y), digits=4)
md.doy.cont<-round(median(datalist.bbdcont$y), digits=4)

mtext(paste(mn.doy.cont))

#Now plot doy~ temp for controls and all
quartz(height=5,width=7)
par(mfrow=c(1,2))
plot(datalist.bbd$temp,datalist.bbd$y, main="All plots",xlab="temp",ylab="BB doy")
abline(a=fixef(testm5.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
plot(datalist.bbdcont$temp,datalist.bbdcont$y, main="Control plots",xlab="temp",ylab="BB doy")
abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5.lmer)[2], col="red")

quartz(height=5,width=7)
par(mfrow=c(1,2))
plot(datalist.bbd$mois,datalist.bbd$y, main="All plots",xlab="vwc",ylab="BB doy")
abline(a=fixef(testm5.lmer)[1],b=fixef(testm5.lmer)[3], col="red")
plot(datalist.bbdcont$mois,datalist.bbdcont$y, main="Control plots",xlab="vwc",ylab="BB doy")
abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5.lmer)[3], col="red")

#Now plot doy~ temp for controls and all
quartz(height=5,width=7)
par(mfrow=c(1,2))
plot(datalist.bbd$temp,datalist.bbd$y, main="All plots",xlab="temp",ylab="BB doy")
abline(a=fixef(testm5.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
plot(datalist.bbdcont$temp,datalist.bbdcont$y, main="Control plots",xlab="temp",ylab="BB doy")
abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
#look at correlation between mois and temp
quartz(height=5,width=7)
par(mfrow=c(1,2))
plot(datalist.bbd$temp,datalist.bbd$mois, main="All plots, r=0.32",xlab="temp",ylab="vwc")
abline(lm(datalist.bbd$mois~datalist.bbd$temp), col="red")
cor(datalist.bbd$mois,datalist.bbd$temp)#0.32
plot(datalist.bbdcont$temp,datalist.bbdcont$mois, main="Control plots, r=0.37",xlab="temp",ylab="vwc")
abline(lm(datalist.bbdcont$mois~datalist.bbdcont$temp), col="red")
cor(datalist.bbdcont$mois,datalist.bbdcont$temp)#0.37


#try the model with brms
#testm5.brms <- brm(y ~ temp * mois +#fixed effects
#                     (temp * mois|sp) + (1|site/year), #random effects
#                   data=datalist.bbd,
#                   chains = 2)# control = list(max_treedepth = 15,adapt_delta = 0.99)

#stancode(testm5.brms)#has 31 divergent transitions
#summary(testm5.brms)
#stanplot(testm5.brms, pars = "^b_")

#stanplot(testm5.brms, surface = TRUE)
#2 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. 

#Fit model with control data only
#testm5cont.brms <- brm(y ~ temp * mois +#fixed effects
##                     (temp * mois|sp) + (1|site/year), #random effects
#                   data=datalist.bbdcont,
#                   chains = 2)# control = list(max_treedepth = 15,adapt_delta = 0.99)


#try the model with brms
testm5cent.brms <- brm(y ~ temp * mois +#fixed effects
                     (temp * mois|sp) + (1|site/year), #random effects
                   data=datalist.bbd.cent,
                   chains = 2,control = list(max_treedepth = 15,adapt_delta = 0.99))
# without control, had divergent transisions and #2 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. but took a really long time to fit...7692.48 seconds (=2.1368 hrs per chain)



stancode(testm5cent.brms)#took 15986.5 seconds for one chain, 15185.4 for the other (~4 hours per chain)
summary(testm5cent.brms)
stanplot(testm5cent.brms, pars = "^b_", title="Budburst model, with species and site/year random effects")
#a: 99.02, temp=--10.40, mois=-1.37, tmint=0.29
save(testm5cent.brms, file="Analyses/output/brms/testm5cent.brms.bb.Rda")
round(fixef(testm5cent.brms, probs=c(.90,0.10)), digits=2)

#make plots of main effects and species- level effects of this model 

quartz()
species=as.numeric(rownames(coef(testm5cent.brms)$sp[,,2]))
plot(coef(testm5cent.brms)$sp[,1,2],1:length(species),type="p",pch=21,bg="darkred",xlab="Temperature effect (days)", ylab=" ", yaxt="n",cex=1.2, xlim=c(-100,20), ylim=c(0,100))
#coef(testm5cent.brms)$sp[,1,2]
abline(v=0)
for (i in 1:length(coef(testm5cent.brms)$sp[,1,2])){
  arrows(coef(testm5cent.brms)$sp[i,3,2],i,coef(testm5cent.brms)$sp[i,4,2],i, code=0)
}
for (i in 1:length(coef(testm5cent.brms)$sp[,1,3])){
  arrows(coef(testm5cent.brms)$sp[i,3,3],i,coef(testm5cent.brms)$sp[i,4,3],i, code=0)
}
points(coef(testm5cent.brms)$sp[,1,2],1:length(species),pch=21,bg="darkred")
points(coef(testm5cent.brms)$sp[,1,3],1:length(species),pch=21,bg="darkblue")

#fixed effects
coefs<-c(95,85,75,65)
for (i in 1:length(fixef(testm5cent.brms)[,1])){
  arrows(fixef(testm5cent.brms)[i,3],coefs[i],fixef(testm5cent.brms)[i,4],coefs[i], code=0)
}
points(fixef(testm5cent.brms)[,1],coefs,pch=21,bg=c("gray","darkred","darkblue","purple4"), cex=2)
axis(2,at=c(95,85,75,65,50),labels=c("intercept","temp","mois","temp*mois","species"), las=2)

#use lizzies code now
load("Analyses/output/brms/testm5cent.brms.bb.Rda")

mod<-testm5cent.brms
sum<-summary(mod)
fix<-sum$fixed
speff <- coef(mod)
rownames(fix)<-c("Intercept","Temperature","Moisture","Temp*Mois")
#pdf(file.path("Analyses/soilmoisture/figures/m5.bbd.pdf"), width = 8, height = 6)
quartz(width = 7, height = 7)
par(mfrow=c(3,1), mar = c(4, 7, .5, 1))
# One panel: budburst
plot(seq(-35, #min(meanz[,'mean']*1.1),
         150, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix)+1, length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of budburst",
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])
for(i in 1:nrow(fix)){
  arrows(speff$sp[,"97.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"2.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha("darkgray", 0.2)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i], 5*(nrow(fix):1)[i]-.5-sp,
         pch = 16,
         col = alpha("darkgray", 0.5))
}
#fixed effects
arrows(fix[,"u-95% CI"], 5*(nrow(fix):1), fix[,"l-95% CI"], 5*(nrow(fix):1),
       len = 0, col = "black", lwd = 2)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 1.2,
       col = "springgreen")

abline(v = 0, lty = 2)
#dev.off()

#Pull out species with strongest effects of moisture
spofint<-c(30,104,98,134,182,25,24,21,1,23,139)
spofit<-sort(spofint)
spofint2<-unique(expgdd_bbd$sp.name[expgdd_bbd$genus.species==spofint])
spofint2<-c("Carya.glabra",spofint2)
spofit2<-sort(spofint2)
ranef(mod)$sp[30,,]
ranef(mod)$sp[rownames(ranef(mod)$sp)=="30",,]
#"Carya.glabra"#30 (-4.341) moisture effect, temo effect=-11.9
#at "exp03" "exp04" Duke and HF

#104 (-6.04) "exp03" "exp04" Duke and HF
ranef(mod)$sp[rownames(ranef(mod)$sp)=="104",,]#"Magnolia.grandiflora"
#98 (-2.94)
#134: 2.10
#182: 2.21
#25: 4.35#Betula.sp (only at Harvard Forest "exp04")
#24: 3.6#Betula.populifolia (only at BACE "exp01")
#21: 4.94#Betula.alleghaniensis (only at Harvard Forest "exp04")
#1: 2.11
# and strongest interacttions:
#
#21: -2.33
#23: -1.37
#104: 1.31
#139: 1.02

#LUD and LOD in lmer
mean(expgdd_bbd$doy, na.rm=TRUE)#99.95649
mean(expgdd_lod$doy, na.rm=TRUE)#145.7179= april/may
summary(expgdd_bbd$doy)
datalist.lod<- with(expgdd_lod, 
                          list(y = doy, 
                               temp = ag_min_aprjun, #above-ground minimum air temp
                               mois = soilmois_aprjun, #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_bbd),
                               n_sp = length(unique(expgdd_bbd$genus.species))
                          )
)


datalist.lod.cent <- with(expgdd_lod, 
                          list(y = doy, 
                               temp = ag_min_aprjun_cent, #above-ground minimum air temp
                               mois = soilmois_aprjun_cent, #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_bbd),
                               n_sp = length(unique(expgdd_bbd$genus.species))
                          )
)


lod.testm5.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site/year),data=datalist.lod)#
summary(lod.testm5.lmer)#failed to converge with uncentered data! 
#temp= -3.8227; mois=-71.7425; temp:mois= 5.1062
lodcent.testm5.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site/year),data=datalist.lod.cent)#
summary(lodcent.testm5.lmer)
#temp= -9.9582; mois=--0.6598; temp:mois= 0.6661
testm5cent.lod.brms <- brm(y ~ temp * mois +#fixed effects
                         (temp * mois|sp) + (1|site/year), #random effects
                       data=datalist.lod.cent,
                       chains = 2,control = list(max_treedepth = 15,adapt_delta = 0.99))
save(testm5cent.lod.brms, file="Analyses/output/brms/testm5cent.brms.lo.Rda")
load("Analyses/output/brms/testm5cent.brms.lo.Rda")
round(fixef(testm5cent.lod.brms, probs=c(.90,0.10)), digits=2)

mod<-testm5cent.lod.brms
sum<-summary(mod)
fix<-sum$fixed
speff <- coef(mod)
rownames(fix)<-c("Intercept","Temperature","Moisture","Temp*Mois")

#pdf(file.path("Analyses/soilmoisture/figures/m5.bbd.pdf"), width = 8, height = 6)
#quartz(width = 8, height = 6)
#par(mfrow=c(1,1), mar = c(6, 10, 2, 1))
# One panel: budburst
plot(seq(-30, 
         200, 
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix)+1, length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of leafout",
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])
for(i in 1:nrow(fix)){
  arrows(speff$sp[,"97.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"2.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha("darkgray", 0.2)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i], 5*(nrow(fix):1)[i]-.5-sp,
         pch = 16,
         col = alpha("darkgray", 0.5))
}
#fixed effects
arrows(fix[,"u-95% CI"], 5*(nrow(fix):1), fix[,"l-95% CI"], 5*(nrow(fix):1),
       len = 0, col = "black", lwd = 2)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 1.2,
       col = "darkgreen")

abline(v = 0, lty = 2)
#dev.off()

#datalist.lud.cent <- with(expgdd_lud, 
 #                         list(y = doy, 
  #                             temp = ag_min_jm_cent, #above-ground minimum air temp
   #                            mois = smjm_cent, #soil moisture
    #                           sp = genus.species,
     #                          site = site,
      #                         year = year,
       #                        N = nrow(expgdd_bbd),
        #                       n_sp = length(unique(expgdd_bbd$genus.species))
         #                 )
#)

#datalist.lud <- with(expgdd_lud, 
#                          list(y = doy, 
#                               temp = ag_min_janmar, #above-ground minimum air temp
#                               mois = soilmois_janmar, #soil moisture
#                               sp = genus.species,
#                               site = site,
#                               year = year,
#                               N = nrow(expgdd_bbd),
#                               n_sp = length(unique(expgdd_bbd$genus.species))
#                          )
#)

#lud.testm5.lmer<-lmer(y~temp * mois +
#                        (temp*mois|sp)+ (1|site/year),
#                      data=datalist.lud)#
#summary(lud.testm5.lmer)#model fits! 
#temp= -7.893; mois=7.655; temp:mois= 18.238
#ludcent.testm5.lmer<-lmer(y~temp * mois +
#                        (temp*mois|sp)+ (1|site/year),
#                      data=datalist.lud.cent)#converged when site variance =3
#summary(ludcent.testm5.lmer)#model fits! 
#temp= -14.0005; mois=0.9292; temp:mois= 2.2634

datalist.ffd.cent <- with(expgdd_ffd, 
                     list(y = doy, 
                          temp = agtmin_cent, #above-ground minimum air temp
                          mois = sm_cent, #soil moisture
                          sp = genus.species,
                          site = site,
                          year = year,
                          N = nrow(expgdd_bbd),
                          n_sp = length(unique(expgdd_bbd$genus.species))
                     )
)

testm5cent.ffd.brms <- brm(y ~ temp * mois +#fixed effects
                             (temp * mois|sp) + (1|site/year), #random effects
                           data=datalist.ffd.cent,
                           chains = 2,control = list(max_treedepth = 15,adapt_delta = .999))

save(testm5cent.ffd.brms, file="Analyses/output/brms/testm5cent.brms.ff.Rda")

mod<-testm5cent.ffd.brms
sum<-summary(mod)
fix<-sum$fixed
speff <- coef(mod)
rownames(fix)<-c("Intercept","Temperature","Moisture","Temp*Mois")

#pdf(file.path("Analyses/soilmoisture/figures/m5.bbd.pdf"), width = 8, height = 6)
#quartz(width = 8, height = 6)
#par(mfrow=c(1,1), mar = c(6, 10, 2, 1))
# One panel: budburst
plot(seq(-25, #min(meanz[,'mean']*1.1),
         300, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix)+1, length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of flowering",
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])
for(i in 1:nrow(fix)){
  arrows(speff$sp[,"97.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"2.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha("darkgray", 0.2)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i], 5*(nrow(fix):1)[i]-.5-sp,
         pch = 16,
         col = alpha("darkgray", 0.5))
}
#fixed effects
arrows(fix[,"u-95% CI"], 5*(nrow(fix):1), fix[,"l-95% CI"], 5*(nrow(fix):1),
       len = 0, col = "black", lwd = 2)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 1.2,
       col = "purple3")

abline(v = 0, lty = 2)
#dev.off()




###For supplement
datalist.ffrd.cent <- with(expgdd_ffrd, 
                          list(y = doy, 
                               temp = agtmin_cent, #above-ground minimum air temp
                               mois = sm_cent, #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_bbd),
                               n_sp = length(unique(expgdd_bbd$genus.species))
                          )
)

testm5cent.ffrd.brms <- brm(y ~ temp * mois +#fixed effects
                             (temp * mois|sp) + (1|site/year), #random effects
                           data=datalist.ffrd.cent,
                           chains = 2,control = list(max_treedepth = 15,adapt_delta = .999))

datalist.sen.cent <- with(expgdd_sen, 
                          list(y = doy, 
                               temp = agtmin_cent, #above-ground minimum air temp
                               mois = sm_cent, #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_bbd),
                               n_sp = length(unique(expgdd_bbd$genus.species))
                          )
)

testm5cent.sen.brms <- brm(y ~ temp * mois +#fixed effects
                              (temp * mois|sp) + (1|site/year), #random effects
                            data=datalist.sen.cent,
                            chains = 2,control = list(max_treedepth = 15,adapt_delta = .999))


ffd.testm5.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site/year),data=datalist.ffd.cent)#converged when site variance =3
summary(ffd.testm5.lmer)#model fits! 
#temp= -7.1193; mois=-2.4218 ; temp:mois= -2.4283


ffrd.testm5.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site/year),data=datalist.ffrd.cent)#converged when site variance =3
summary(ffrd.testm5.lmer)#model fits! 
#temp= -5.1190; mois=-0.3049 ; temp:mois= -0.7659 

sen.testm5.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site/year),data=datalist.sen.cent)#converged when site variance =3
summary(sen.testm5.lmer)#model fits! 
#temp= 1.311 ; mois=-8.317 ; temp:mois= -4.695

##Figure of FFRD and SEN
mod<-testm5cent.ffrd.brms 
sum<-summary(mod)
fix<-sum$fixed
speff <- coef(mod)
rownames(fix)<-c("Intercept","Temperature","Moisture","Temp*Mois")
#pdf(file.path("Analyses/soilmoisture/figures/m5.bbd.pdf"), width = 8, height = 6)
quartz(width = 8, height = 7)
par(mfrow=c(2,1), mar = c(4, 7, .5, 1))
# One panel: fruiting
plot(seq(-45, #min(meanz[,'mean']*1.1),
         360, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix)+1, length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of fruiting",
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])
for(i in 1:nrow(fix)){
  arrows(speff$sp[,"97.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"2.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha("darkgray", 0.2)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i], 5*(nrow(fix):1)[i]-.5-sp,
         pch = 16,
         col = alpha("darkgray", 0.5))
}
#fixed effects
arrows(fix[,"u-95% CI"], 5*(nrow(fix):1), fix[,"l-95% CI"], 5*(nrow(fix):1),
       len = 0, col = "black", lwd = 2)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 1.2,
       col = "midnight blue")

abline(v = 0, lty = 2)
#dev.off()

mod<-testm5cent.sen.brms 
sum<-summary(mod)
fix<-sum$fixed
speff <- coef(mod)
rownames(fix)<-c("Intercept","Temperature","Moisture","Temp*Mois")
#pdf(file.path("Analyses/soilmoisture/figures/m5.sen.pdf"), width = 8, height = 6)

# One panel: senescence
plot(seq(-45, #min(meanz[,'mean']*1.1),
         360, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix)+1, length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of senescence",
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])
for(i in 1:nrow(fix)){
  arrows(speff$sp[,"97.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"2.5%ile",i],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha("darkgray", 0.2)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i], 5*(nrow(fix):1)[i]-.5-sp,
         pch = 16,
         col = alpha("darkgray", 0.5))
}
#fixed effects
arrows(fix[,"u-95% CI"], 5*(nrow(fix):1), fix[,"l-95% CI"], 5*(nrow(fix):1),
       len = 0, col = "black", lwd = 2)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 1.2,
       col = "burlywood4")

abline(v = 0, lty = 2)
#dev.off()

#Now explore the implications of this model a bit:
#To do:
#1) Does soil moisture have a stronger effect in experimental than non experimental data?
#Why might it? possibly because experiments are often with smaller/younger stages/less developed root systems whereas observational studies are 
#more likely to be of adult individuals with well-developed roots.
#Are individuals in the studies in our data set small? large? young/old?
unique(expgdd_subs$site2[expgdd_subs$event=="bbd"])
unique(expgdd_subs$site2[expgdd_subs$event=="lod"])

#"exp01" "exp03" "exp04" "exp07" "exp10"
#What stage/age/size are plants in these experiments?
bbdset<-expgdd_subs[expgdd_subs$event=="bbd",]
# exp01- has a mix of established plants (forbs and grasses) 
# and planted tree seedlings (Acer rubrum, Betula lenta, Pinus strobus, Quercus rubra)
# exp03: tree seeds sowed
# exp04: tree seeds sowed
# exp07: pre-existing vegetation
# exp10: pre-existing vegetation
table(bbdset$site2,bbdset$styear)
table(expgdd_subs$sp.name[expgdd_subs$site2 =="exp01"],expgdd_subs$year[expgdd_subs$site2 =="exp01"])

#does effect of soil moisture diminish with time (for bace)?






#Old stan model code
#################################################################
# m1: a(sp) + t(sp) + m(sp) + t*m(sp) (ran eff only of species) #
#################################################################
m1 = stan('Analyses/soilmoisture/M1_bbd.stan', data = datalist.bbd,
               iter = 2500, warmup=1500) # 
 
save(m1, file="Analyses/soilmoisture/M1_bbd.rda")
#load("Analyses/soilmoisture/M1_bbd.Rda")
m1.sum <- summary(m1)$summary 
#head(m1.sum) 
m1.sum[grep("mu_a_sp", rownames(m1.sum)),]
m1.sum[grep("b_temp", rownames(m1.sum)),]
m1.sum[grep("b_mois", rownames(m1.sum)),]
m1.sum[grep("b_tm", rownames(m1.sum)),]
launch_shinystan(m1)
#################################################################
# m2: a(sp) +a(site)+ t(sp) + m(sp) + t*m(sp) (crossed ran eff of species and site) #
#################################################################
#2) Make a list out of the processed data. It will be input for the model.
####The below model is not ready yet!!!!
datalist2.bbd <- with(expgdd_bbd, 
                     list(y = doy, 
                          temp = ag_min_janmar, #above-ground minimum air temp
                          mois = soilmois_janmar, #soil moisture
                          sp = genus.species,
                          site = site,
                          N = nrow(expgdd_bbd),
                          n_sp = length(unique(expgdd_bbd$genus.species)),
                          n_site = length(unique(expgdd_bbd$site))
                     )
)
m2 = stan('Analyses/soilmoisture/M2_bbd.stan', data = datalist.bbd,
          iter = 2500, warmup=1500) # 

save(m2, file="Analyses/soilmoisture/M2_bbd.rda")
#load("Analyses/soilmoisture/M2_bbd.Rda")
m2.sum <- summary(m1)$summary 
head(m2.sum) 
m2.sum[grep("mu_a_sp", rownames(m2.sum)),]
m2.sum[grep("b_temp", rownames(m2.sum)),]
m2.sum[grep("b_mois", rownames(m2.sum)),]
m2.sum[grep("b_tm", rownames(m2.sum)),]


#data=list(y=y,sp=sp,site=site,temp=temp, mois=mois,n_sp=n_sp,n_site=n_site,N=N)) 
datalist.bbd <- with(expgdd_bbd, 
                      list(y = doy, 
                           sp = genus.species,
                           site = site,
                           temp = temp = ag_min_jm_cent[,1], #above-ground minimum air temp
                           mois = smjm_cent[,1], #soil moisture as a proportion
                           n_sp = length(unique(expgdd_bbd$genus.species)),
                           n_site = length(unique(expgdd_bbd$site)),
                           N = nrow(expgdd_bbd)
                           )
                      )

m4 = stan('Analyses/soilmoisture/M4_bbd_testdata.stan', data = datalist.bbd,
          iter = 2500, warmup=1500) # 




#1) How do warming and precip treatments affect temperature and soil moisture? (Make plots and fit models)

expclim2$target_cent<-scale(expclim2$target, center = TRUE, scale = TRUE)
expclim2$preciptreat_amt_cent<-scale(expclim2$preciptreat_amt, center = TRUE, scale = TRUE)
expclim2a<-subset(expclim2,select=c(site,year,doy,target_cent,preciptreat_amt,target,preciptreat_amt_cent,soilmois1,agtemp_mean))
expclim2a<- expclim2a [apply(expclim2a , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
expclim2a$doy<-as.factor(expclim2a$doy)
expclim2a$year<-as.factor(expclim2a$year)
expclim2a$site<-as.factor(expclim2a$site)
expclim2a$preciptreat_prop<-expclim2a$preciptreat_amt/100

#select out only sites that manip both temp and precip
expclim3<-expclim2a[expclim2a$site=="exp01"|expclim2a$site=="exp05"|expclim2a$site=="exp09"|expclim2a$site=="exp12",]
#expclim3$target_cent<-scale(expclim3$target, center = TRUE, scale = TRUE)
#expclim3$preciptreat_amt_cent<-scale(expclim3$preciptreat_amt, center = TRUE, scale = TRUE)
expclim3<- expclim3 [apply(expclim3 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na

#fit model in lmer
###Fit lmer model for soil moisture~warming*preciptreatment
sm_mod<-lmer(soilmois1~target*preciptreat_prop + (target*preciptreat_prop|site)+(1|year/doy), REML=FALSE, data=expclim3)
summary(sm_mod)#doesn't converge for preciptreat_amt or precipttreat_prop with expclim2a but DOES for expclim3

smtemp_mod<-lmer(soilmois1~target + (target|site)+(1|year/doy), REML=FALSE, data=expclim2a)
summary(smtemp_mod)

smprecip_mod<-lmer(soilmois1~preciptreat_prop + (preciptreat_prop|site)+(1|year/doy), REML=FALSE, data=expclim2a)
summary(smprecip_mod)

#sm_mod_cent<-lmer(soilmois1~target_cent*preciptreat_amt_cent + 
#                    (target_cent*preciptreat_amt_cent|site)+(1|year/doy),
#                 REML=FALSE, data=expclim2a)
#summary(sm_mod_cent)#convergence warning for both expclim3 and expclim 2a

#sm_mod_cent.b<-brm(soilmois1~target_cent*preciptreat_amt_cent + 
#             ((target_cent*preciptreat_amt_cent)|site)+(1|year/doy),
#            data=expclim3)#
sm_mod.b<-brm(soilmois1~target*preciptreat_prop + 
                (target*preciptreat_prop|site)+(1|year/doy),
              data=expclim3,
              chains = 2)#

smtemp_mod.b<-brm(soilmois1~target + 
                    (target|site)+(1|year/doy),
                  data=expclim2a)#
smprecip_mod.b<-brm(soilmois1~preciptreat_prop + 
                      (preciptreat_prop|site)+(1|year/doy),
                    data=expclim2a)#

#summary(sm_mod_cent.b)
#temp mod
temp_mod_cent<-lmer(agtemp_mean ~ target_cent*preciptreat_amt_cent +#fixed effects
                      (target_cent*preciptreat_amt_cent|site) + (1|year/doy), #random effects
                    data=expclim2a)# control = list(max_treedepth = 15,adapt_delta = 0.99)

#fit model with brms
temp_mod.b<-brm(agtemp_mean ~ target*preciptreat_prop +#fixed effects
                  (target*preciptreat_prop|site) + (1|year/doy), #random effects
                data=expclim3,
                chains = 2)# control = list(max_treedepth = 15,adapt_delta = 0.99)

summary(sm_mod_cent.b)
#save coefficients from models to make a table
#table<-cbind(fixef(temp_mod_cent.b),fixef(sm_mod_cent.b))
table<-cbind(summary(temp_mod_cent)$coefficients[,1:2],summary(sm_mod_cent)$coefficients[,1:2])
table<-round(table, digits=3)
#sm_tempmod_cent<-lmer(soilmois1~target_cent + (target_cent|site)+(1|year/doy), REML=FALSE, data=expclim2a)
#summary(sm_tempmod_cent)#model does not fit for noncentered data
#AIC(sm_tempmod_cent,sm_mod_cent)
colnames(table)<-c("Tmod.coef","Tmod.se","SMmod.coef","SMmod.se")
rownames(table)<-c("int","temp.treat","precip.treat","temp.treat*precip.treat")
write.csv(table,"Analyses/soilmoisture/tempmoislmer.csv", row.names = TRUE)


#Figure with lines for each site showing different effects
quartz(height=7, width=7)
par(mfrow=c(2,2))
#first air temp
plot(expclim2a$target_cent[1],expclim2a$agtemp_mean[1],type="p", pch=21,col="white", ylab="Above-ground temperature", xlab="Target warming (centered)", bty="l", xlim=c(-1.5,2.5),ylim=c(0,30))
#site effects
cols <- brewer.pal(12,"Set3")

for(i in 1:dim(coef(temp_mod_cent)$site)[1]){
  abline(a=coef(temp_mod_cent)$site[i,1],b=coef(temp_mod_cent)$site[i,2], lwd=1, col= cols[i])
}
#main effects
abline(a=fixef(temp_mod_cent)[1],b=fixef(temp_mod_cent)[2], lwd=2)

plot(expclim2a$preciptreat_amt_cent[1],expclim2a$agtemp_mean[1],type="p", pch=21,col="white", ylab="Above-ground temperature", xlab="Precipitation treatment (centered)", bty="l", xlim=c(-2.5,5),ylim=c(0,30))
#site effects
for(i in 1:dim(coef(temp_mod_cent)$site)[1]){
  abline(a=coef(temp_mod_cent)$site[i,1],b=coef(temp_mod_cent)$site[i,3], lwd=1, col= cols[i])
}
abline(a=fixef(temp_mod_cent)[1],b=fixef(temp_mod_cent)[3], lwd=2)

#soil moisture
plot(expclim2a$target_cent[1],expclim2$soilmois1[1],type="p", pch=21,col="white", ylab="Soil moisture", xlab="Target warming (centered)", bty="l", ylim=c(0,0.4),xlim=c(-1.5,2.5))

for(i in 1:dim(coef(sm_mod_cent)$site)[1]){
  abline(a=coef(sm_mod_cent)$site[i,1],b=coef(sm_mod_cent)$site[i,2], lwd=1, col= cols[i])
}
#main effects
abline(a=fixef(sm_mod_cent)[1],b=fixef(sm_mod_cent)[2], lwd=2)

plot(expclim2a$preciptreat_amt_cent[1],expclim2a$soilmois1[1],type="p", pch=21,col="white", ylab="Soil moisture", xlab="Precipitation treatment (centered)", bty="l", xlim=c(-2.5,5),ylim=c(0,0.4))
#site effects
for(i in 1:dim(coef(sm_mod_cent)$site)[1]){
  abline(a=coef(sm_mod_cent)$site[i,1],b=coef(sm_mod_cent)$site[i,3], lwd=1, col= cols[i])
}
#main effects
abline(a=fixef(sm_mod_cent)[1],b=fixef(sm_mod_cent)[3], lwd=2)
#Need to add interaction somehow..
sites<-unique(expclim2a$site)
op<-par(cex=.4)
legend("bottomright",legend=paste(sites),lwd=1,col=cols[1:length(sites)], bty="n")

expclim2$preciptreat_prop<-expclim2$preciptreat_amt/100
#now fit a model of
#target warming:
sm_mod_targ<-lmer(soilmois1~target*preciptreat_prop + (target*preciptreat_prop|site)+(1|year/doy), REML=FALSE, data=expclim2)
summary(sm_mod_targ)

#measured warming:
sm_mod_meas<-lmer(soilmois1~agtemp_mean*preciptreat_prop + (agtemp_mean*preciptreat_prop|site)+(1|year/doy), REML=FALSE, data=expclim2)
summary(sm_mod_meas)
###########################
############################
#Fit models of soilmois~measured warming with predictor of treatment type as well 
#use measured warming and predictor for treatment (control, warmed only, warmedwith precip)
###########################
###########################
#create a new varibale for treatment type (ttype)
expclim2$ttype<-"cont"
expclim2$ttype[as.numeric(expclim2$temptreat)>0]<-"warmprecip"
expclim2$ttype[expclim2$preciptreat==-1 ]<-"warmprecip"
expclim2$ttype[expclim2$preciptreat==1]<-"warmprecip"
expclim2$ttype2<-"cont"
expclim2$ttype2[as.numeric(expclim2$temptreat)>0]<-"warm"
expclim2$ttype2[expclim2$preciptreat==-1 ]<-"precip"
expclim2$ttype2[expclim2$preciptreat==1]<-"precip"

#do we want to separate out precip only treatments?
expclim2$ttype<-as.factor(expclim2$ttype)
expclim2$ttype2<-as.factor(expclim2$ttype2)

expclim2$agtemp_mean<-as.numeric(expclim2$agtemp_mean)
expclim2$soilmois1<-as.numeric(expclim2$soilmois1)
#divide into 2 different datasets
expclim_cont<-expclim2[expclim2$ttype=="cont",]
expclim_treat<-expclim2[expclim2$ttype=="warmprecip",]
#remove exp02, which doesn't measure airtemp
expclim_cont<-expclim_cont[!expclim_cont$site=="exp02",]
expclim_treat<-expclim_treat[!expclim_treat$site=="exp02",]

#expclim_precip<-expclim2[expclim2$ttype=="precip",]

sm_mod_temptreat<-lmer(soilmois1~agtemp_mean*ttype + (agtemp_mean*ttype|site)+(1|year/doy), REML=FALSE, data=expclim2)
summary(sm_mod_temptreat)#no convergence
#Fit separate model for each treatment type for now?

sm_mod_temptreat_cont<-lmer(soilmois1~agtemp_mean+ (agtemp_mean|site)+(1|year/doy), REML=FALSE, data=expclim_cont)
summary(sm_mod_temptreat_cont)

sm_mod_temptreat_treat<-lmer(soilmois1~agtemp_mean + (agtemp_mean|site)+(1|year/doy), REML=FALSE, data=expclim_treat)
summary(sm_mod_temptreat_treat)

#sm_mod_temp.brms<-brm(soilmois1~agtemp_mean 
#                      + (agtemp_mean|site)+(1|year/doy), 
#                     data=expclim2)
#summary(sm_mod_temp.brms)

#Make a plot of soilmois~agtemp, with lines the length of measured climate
#different lines for each site
sites_cont<-rownames(ranef(sm_mod_temptreat_cont)$site)
sites_treat<-rownames(ranef(sm_mod_temptreat_treat)$site)

#the sites are the same
#different sites get different colors
cols <- brewer.pal(8,"Set3")

quartz(height=7, width=7)
plot(expclim_cont$target_cent[1],expclim_cont$agtemp_mean[1],type="p", pch=21,col="white", ylab="Soil moisture", xlab="Above-ground temperature", bty="l", xlim=c(min(expclim2$agtemp_mean, na.rm=TRUE)-.5,max(expclim2$agtemp_mean, na.rm=TRUE)),ylim=c(-0,0.5))

#add lines for models fit to control plots
for(i in 1:length(sites_cont)){
  site.mod<-coef(sm_mod_temptreat_cont)$site[i,]
  xsitemax<-max(expclim_cont$agtemp_mean[expclim_cont==sites_cont[i]], na.rm=TRUE) 
  xsitemin<-min(expclim_cont$agtemp_mean[expclim_cont==sites_cont[i]], na.rm=TRUE) 
  ysitemax<-max(expclim_cont$soilmois1[expclim_cont==sites_cont[i]], na.rm=TRUE) 
  ysitemin<-min(expclim_cont$soilmois1[expclim_cont==sites_cont[i]], na.rm=TRUE) 
  ablineclip(a=as.numeric(site.mod[1]),b=as.numeric(site.mod[2]),x1=xsitemin,x2=xsitemax,y1=-0.5,y2=1.0,col=cols[i])
  print(site.mod);
  print(xsitemax); print(xsitemin);
  
  #xvals<-as.vector(seq(xsitemin,xsitemax))
  #yvals<-as.numeric(site.mod[2])*(xvals)+as.numeric(site.mod[1])
  #lines(xvals,yvals,col=cols[i], lty=3)
}
#main line for control plots
xmax<-max(expclim_cont$agtemp_mean, na.rm=TRUE) 
xmin<-min(expclim_cont$agtemp_mean, na.rm=TRUE) 

ablineclip(fixef(sm_mod_temptreat_cont),lwd=2,col="black", x1=xmin, x2=xmax)
#now add treatment plots
for(i in 1:length(sites_treat)){
  site.mod<-coef(sm_mod_temptreat_treat)$site[i,]
  xsitemax<-max(expclim_treat$agtemp_mean[expclim_treat==sites_treat[i]], na.rm=TRUE) 
  xsitemin<-min(expclim_treat$agtemp_mean[expclim_treat==sites_treat[i]], na.rm=TRUE) 
  ysitemax<-max(expclim_treat$soilmois1[expclim_treat==sites_treat[i]], na.rm=TRUE) 
  ysitemin<-min(expclim_treat$soilmois1[expclim_treat==sites_treat[i]], na.rm=TRUE) 
  ablineclip(a=as.numeric(site.mod[1]),b=as.numeric(site.mod[2]),x1=xsitemin,x2=xsitemax,y1=-0.5,y2=1.0,col=cols[i], lty=2)
  print(site.mod);
  print(xsitemax); print(xsitemin);
}
xmax2<-max(expclim_treat$agtemp_mean, na.rm=TRUE) 
xmin2<-min(expclim_treat$agtemp_mean, na.rm=TRUE) 

ablineclip(fixef(sm_mod_temptreat_treat),lwd=2,col="black", lty=2, x1=xmin2, x2=xmax2)
###############
#Try divide into 3 different datasets
expclim_cont2<-expclim2[expclim2$ttype2=="cont",]
expclim_warm<-expclim2[expclim2$ttype2=="warm",]

#remove exp02, which doesn't measure airtemp
expclim_cont2<-expclim_cont2[!expclim_cont2$site=="exp02",]
expclim_warm<-expclim_warm[!expclim_warm$site=="exp02",]





#########Exploring#########

sm_mod_temp_cont<-lmer(soilmois1~agtemp_mean + (agtemp_mean|site)+(1|year/doy), REML=FALSE, data=expclim_cont)
summary(sm_mod_temp_cont)

modcomp<-rbind(fixef(sm_mod_targ)[,1:2],fixef(sm_mod_meas)[,1:2],c(fixef(sm_mod_temp)[,1],NA,NA),c(fixef(sm_mod_temp)[,2],NA,NA),c(fixef(sm_mod_temp_cont)[,1],NA,NA),c(fixef(sm_mod_temp_cont)[,2],NA,NA))

colnames(modcomp)<-c("eff","se")

