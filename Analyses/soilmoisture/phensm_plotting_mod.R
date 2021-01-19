### Make a Figure of the Model Estimates  for Soil Moisture Paper
### Code borrowed heavily from OSPREE!
### By Ailene Ettinger
### ailene.ettinger@tnc.org
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

#options(mc.cores = parallel::detectCores()) # added by Andrew
#update.packages()

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/radcliffe")}
#setwd("~/Documents/GitHub/radcliffe")#noaa
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
source("Analyses/source/stanprep_phenmods.R")

#get sp.name/number set up
splegbb<- expgdd_bbd %>% # start with the data frame
              distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
              dplyr::select(sp.name,genus.species)
splegbb<-splegbb[order(splegbb$genus.species),]
colnames(splegbb)[2]<-"spnumbb"

spleglo<-expgdd_lod %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)    
spleglo<-spleglo[order(spleglo$genus.species),]
colnames(spleglo)[2]<-"spnumlo"

splegfl<-expgdd_ffd %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)    
splegfl<-splegfl[order(splegfl$genus.species),]
colnames(splegfl)[2]<-"spnumfl"

splegall<-full_join(splegbb,spleglo,by = "sp.name")
splegall2<-full_join(splegall,splegfl,by = "sp.name")
load("Analyses/output/brms/testm5cent.brms.bb.Rda")#need to rerun the centered model- divergent transitions
#load("Analyses/output/brms/testm5.brms.bb.Rda")#no divergent transistions

mod<-testm5cent.brms
sum<-summary(mod)
fix<-sum$fixed[2:4,]
speff <- coef(mod)
rownames(fix)<-c("Temperature","Moisture","Temp*Mois")
#pdf(file.path("Analyses/soilmoisture/figures/m5.bbd.pdf"), width = 8, height = 6)
quartz(width = 7, height = 10)
par(mfrow=c(2,1), mar = c(10, 10, 5, 10))

# One panel: budburst
minx<-min(speff$sp[,2:4,2:4])
maxx<-max(speff$sp[,2:4,2:4])
#minx<--20
#maxx<-20
plot(seq(minx, #min(meanz[,'mean']*1.1),
         maxx, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix), length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of budburst",
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])

#colors differ by species
nsp<-dim(speff$sp)[1]
my.pal <- rep(brewer.pal(n = 12, name = "Set3"), 4)[1:nsp]
my.pch <- rep(15:18, each=12)[1:nsp]
  
for(i in 1:nrow(fix)){
  arrows(speff$sp[,"Q97.5",i+1],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"Q2.5",i+1],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha(my.pal, 0.5)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i+1], 5*(nrow(fix):1)[i]-.5-sp,
         pch = my.pch,
         col = alpha(my.pal, 0.5))
}
#fixed effects
arrows(fix[,"u-95% CI"], 5*(nrow(fix):1), fix[,"l-95% CI"], 5*(nrow(fix):1),
       len = 0, col = "darkblue", lwd = 2)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 1.5,
       col = "darkblue")

abline(v = 0, lty = 2)

par(xpd=TRUE) # so I can plot legend outside
leg1<-maxx+6
leg2<-5*(nrow(fix):1)[1]+7
legend(leg1, leg2, splegbb$sp.name,
       pch=my.pch,
       col=alpha(my.pal, .5),
       bty = "n",
       cex=0.50, text.font=3)

#dev.off()

load("Analyses/output/brms/testm5cent.brms.lo.Rda")

mod<-testm5cent.lod.brms
sum<-summary(mod)
fix<-sum$fixed[2:4,]
speff <- coef(mod)
rownames(fix)<-c("Temperature","Moisture","Temp*Mois")

minx<-min(speff$sp[,2:4,2:4])
maxx<-max(speff$sp[,2:4,2:4])
#minx<--20
#maxx<-20
plot(seq(minx, #min(meanz[,'mean']*1.1),
         maxx, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix), length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of leafout",
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])

#colors differ by species
nsp<-dim(speff$sp)[1]
my.pal <- rep(brewer.pal(n = 12, name = "Set3"), 4)[1:nsp]
my.pch <- rep(15:18, each=12)[1:nsp]

for(i in 1:nrow(fix)){
  arrows(speff$sp[,"Q97.5",i+1],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"Q2.5",i+1],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha(my.pal, 0.5)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i+1], 5*(nrow(fix):1)[i]-.5-sp,
         pch = my.pch,
         col = alpha(my.pal, 0.5))
}
#fixed effects
arrows(fix[,"u-95% CI"], 5*(nrow(fix):1), fix[,"l-95% CI"], 5*(nrow(fix):1),
       len = 0, col = "darkgreen", lwd = 2)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 1.5,
       col = "darkgreen")

abline(v = 0, lty = 2)
#dev.off()



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



quartz()
species=as.numeric(rownames(coef(mod)$genus.species[,,2]))
plot(coef(mod)$genus.species[,1,2],1:length(species),type="p",pch=21,bg="darkred",xlab="Temperature effect (days)", ylab=" ", yaxt="n",cex=1.2, xlim=c(-100,20), ylim=c(0,100))
#coef(testm5cent.brms)$sp[,1,2]
abline(v=0)
for (i in 1:length(coef(mod)$genus.species[,1,2])){
  arrows(coef(mod)$genus.species[i,3,2],i,coef(mod)$genus.species[i,4,2],i, code=0)
}
for (i in 1:length(coef(mod)$sgenus.species[,1,3])){
  arrows(coef(mod)$sp[i,3,3],i,coef(mod)$sgenus.species[i,4,3],i, code=0)
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

load("Analyses/output/brms/testm5.brms.bb.Rda")

