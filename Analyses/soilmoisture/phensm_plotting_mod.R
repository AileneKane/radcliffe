### Make a Figure of the Model Estimates  for Soil Moisture Paper
### Code borrowed heavily from OSPREE!
### By Ailene Ettinger
### ailene.ettinger@tnc.org
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE
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
if(length(grep("ailene", getwd()))>0) {setwd("~/GitHub/radcliffe")}#Tnc

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
table(expgdd_bbd$site)
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
splegall<-splegall[order(splegall$sp.name),]

splegall2<-full_join(splegall,splegfl,by = "sp.name")

load("Analyses/output/brms/testm5cent.brms.bb.Rda")
#load("Analyses/output/brms/testm5.brms.bb.Rda")#no divergent transistions

mod<-testm5cent.brms
bbmod<-testm5cent.brms

sum<-summary(mod, prob =.8)
fix<-sum$fixed[2:4,]
speffbb <- coef(mod, probs = c(.10,.90))
rownames(fix)<-c("Temperature","Moisture","Temp*Mois")
pdf(file.path("Analyses/soilmoisture/figures/m5_bbdlofl.pdf"), width = 15, height = 5)
#windows(width = 10, height = 10)
par(mfrow=c(1,3), mar = c(5, 10, 2, 15))

# One panel: budburst
minx<-min(speffbb$sp[,2:4,2:4])
maxx<-max(speffbb$sp[,2:4,2:4])
#minx<--20
#maxx<-20
plot(seq(minx, #min(meanz[,'mean']*1.1),
         maxx, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix), length.out = nrow(fix)),
     type="n",
     main="Budburst",
     xlab = "Model estimate, change in day of budburst",
     #xlim =c(-90,40),
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speffbb$sp)[1])/dim(speffbb$sp)[1])

#colors differ by species
nsp<-dim(speffbb$sp)[1]
nspall<-dim(splegall)[1]
my.pal <- rep(brewer.pal(n = 12, name = "Set3"), 13)[1:nsp]
my.pch <- rep(21:25, each=30)[1:nsp]
greens<-brewer.pal(8,"Greens")
for(i in 1:nrow(fix)){
  arrows(speffbb$sp[,"Q90",i+1],  5*(nrow(fix):1)[i]-.5-sp, speffbb$sp[,"Q10",i+1],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha(my.pal, 0.5)) 
}
for(i in 1:nrow(fix)){
  points(speffbb$sp[,"Estimate",i+1], 5*(nrow(fix):1)[i]-.5-sp,
         pch = my.pch,
         col = alpha(my.pal, 0.5),
          bg=alpha(my.pal, .5))
}
#fixed effects
arrows(fix[,"u-80% CI"], 5*(nrow(fix):1), fix[,"l-80% CI"], 5*(nrow(fix):1),
       len = 0, col = greens[7], lwd = 3)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 2.5,
       col = greens[7])

abline(v = 0, lty = 2)
mtext("A)", side=3, line =0,adj=-.3)
par(xpd=TRUE) # so I can plot legend outside
leg1<-maxx+5
leg2<-5*(nrow(fix):1)[1]+1
legend(leg1, leg2, splegbb$sp.name,
       pch=my.pch,
       col=alpha(my.pal, .5),
       pt.bg=alpha(my.pal, .5),
       bty = "n",
       cex=0.70, text.font=3)

#dev.off()

load("Analyses/output/brms/testm5cent.brms.lo.Rda")

mod<-testm5cent.lod.brms
lomod<-testm5cent.lod.brms

sum<-summary(mod, prob =.8)
fix<-sum$fixed[2:4,]
speff <- coef(mod, probs=c(0.10,.90))
spefflo <- coef(mod, probs=c(0.10,.90))

rownames(fix)<-c("Temperature","Moisture","Temp*Mois")

minx<-min(speff$sp[,2:4,2:4])
maxx<-max(speff$sp[,2:4,2:4])

plot(seq(minx, #min(meanz[,'mean']*1.1),
         maxx, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix), length.out = nrow(fix)),
     type="n",
     main="Leafout",
     xlab = "Model estimate, change in day of leafout",
     #xlim =c(-90,40),
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)
mtext("B)", side=3, line =0,adj=-.3)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])

#colors differ by species
nsp<-dim(speff$sp)[1]
my.pal <- rep(brewer.pal(n = 12, name = "Set3"), 13)[1:nsp]
my.pch <- rep(21:25, each=30)[1:nsp]
for(i in 1:nrow(fix)){
  arrows(speff$sp[,"Q90",i+1],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"Q10",i+1],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha(my.pal, 0.5)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i+1], 5*(nrow(fix):1)[i]-.5-sp,
         pch = my.pch,
         bg=alpha(my.pal, .5),
         col = alpha(my.pal, 0.5))
}
#fixed effects
arrows(fix[,"u-80% CI"], 5*(nrow(fix):1), fix[,"l-80% CI"], 5*(nrow(fix):1),
       len = 0, col = greens[8], lwd = 3)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 2.5,
       col = greens[8])
par(xpd=FALSE) # so line stays within plot bounds

abline(v = 0, lty = 2)
par(xpd=TRUE) # so I can plot legend outside
leg1<-maxx+3
leg2<-5*(nrow(fix):1)[1]+1
leg1a<-maxx+30

legend(leg1, leg2, spleglo$sp.name[1:as.integer(length(spleglo$sp.name)/2)+1],
       pch=my.pch[1:as.integer(length(spleglo$sp.name)/2)+1],
       col=alpha(my.pal, .5)[1:as.integer(length(spleglo$sp.name)/2)+1],
       pt.bg=alpha(my.pal, .5)[1:as.integer(length(spleglo$sp.name)/2)+1],
       bty = "n",
       cex=0.45, text.font=3)
legend(leg1a, leg2, spleglo$sp.name[(as.integer(length(spleglo$sp.name)/2)+2):length(spleglo$sp.name)],
       pch=my.pch[(as.integer(length(spleglo$sp.name)/2)+2):length(spleglo$sp.name)],
       col=alpha(my.pal, .5)[(as.integer(length(spleglo$sp.name)/2)+2):length(spleglo$sp.name)],
       pt.bg=alpha(my.pal, .5)[(as.integer(length(spleglo$sp.name)/2)+2):length(spleglo$sp.name)],
       bty = "n",
       cex=0.45, text.font=3)
#dev.off()

#Flowering
load("Analyses/output/brms/testm5cent.brms.ff.Rda")
mod<-testm5cent.ffd.brms
flmod<-testm5cent.ffd.brms

sum<-summary(flmod, prob =.8)
fix<-sum$fixed[2:4,]
speff <- coef(mod, probs=c(0.10,.90))
spefffl <- coef(mod, probs=c(0.10,.90))

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
     main="Flowering",
     xlab = "Model estimate, change in day of flowering",
     #xlim =c(-90,40),
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)
mtext("C)", side=3, line =0,adj=-.3)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])

#colors differ by species
nsp<-dim(speff$sp)[1]
nspall<-dim(splegall)[1]
my.pal <- rep(brewer.pal(n = 12, name = "Set3"), 13)[1:nsp]
my.pch <- rep(21:25, each=30)[1:nsp]
greens<-brewer.pal(8,"Greens")
for(i in 1:nrow(fix)){
  arrows(speff$sp[,"Q90",i+1],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"Q10",i+1],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha(my.pal, 0.5)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i+1], 5*(nrow(fix):1)[i]-.5-sp,
         pch = my.pch,
         col = alpha(my.pal, 0.5),
         bg=alpha(my.pal, .5))
}
#fixed effects
arrows(fix[,"u-80% CI"], 5*(nrow(fix):1), fix[,"l-80% CI"], 5*(nrow(fix):1),
       len = 0, col = "purple3", lwd = 3)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 2.5,
       col = "purple3")
par(xpd=FALSE) # so line stays within plot bounds

abline(v = 0, lty = 2)

par(xpd=TRUE) # so I can plot legend outside
leg1<-maxx+2
leg2<-5*(nrow(fix):1)[1]+1
leg1a<-maxx+25

legend(leg1, leg2, splegfl$sp.name[1:as.integer(length(splegfl$sp.name)/2)+1],
       pch=my.pch[1:as.integer(length(splegfl$sp.name)/2)+1],
       col=alpha(my.pal, .5)[1:as.integer(length(splegfl$sp.name)/2)+1],
       pt.bg=alpha(my.pal, .5)[1:as.integer(length(splegfl$sp.name)/2)+1],
       bty = "n",
       cex=0.45, text.font=3)
legend(leg1a, leg2, splegfl$sp.name[(as.integer(length(splegfl$sp.name)/2)+2):length(splegfl$sp.name)],
       pch=my.pch[(as.integer(length(splegfl$sp.name)/2)+2):length(splegfl$sp.name)],
       col=alpha(my.pal, .5)[(as.integer(length(splegfl$sp.name)/2)+2):length(splegfl$sp.name)],
       pt.bg=alpha(my.pal, .5)[(as.integer(length(splegfl$sp.name)/2)+2):length(splegfl$sp.name)],
       bty = "n",
       cex=0.45, text.font=3)

dev.off()


###Plot intercepts vs slopes of species effects











###Panel 2 Fruiting
load("Analyses/output/brms/testm5cent.brms.frd.Rda")

mod<-testm5cent.ffrd.brms 
sum<-summary(mod,prob =.8)
fix<-sum$fixed
speff <- coef(mod)
rownames(fix)<-c("Intercept","Temperature","Moisture","Temp*Mois")
#pdf(file.path("Analyses/soilmoisture/figures/m5.bbd.pdf"), width = 8, height = 6)
#quartz(width = 8, height = 7)
#par(mfrow=c(2,1), mar = c(4, 7, .5, 1))
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
  arrows(speff$sp[,"Q97.5",i],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"Q2.5",i],  5*(nrow(fix):1)[i]-.5-sp,
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
load("Analyses/output/brms/testm5cent.brms.sen.Rda")
mod<-testm5cent.sen.brms 
sum<-summary(mod,prob =.8)
fix<-sum$fixed
speff <- coef(mod)$sp
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
  arrows(speff$sp[,"Q97.5",i],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"Q2.5",i],  5*(nrow(fix):1)[i]-.5-sp,
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
dev.off()




#Make figures with reduced dataset (only those that are the same between lo and bb, anbd between lo and fl)
#get sp.name/number set up
splegbb<- expgdd_bbd%>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)
splegbb<-splegbb[order(splegbb$sp.name),]
colnames(splegbb)[2]<-"spnumbb"

spleglofl<-expgdd_lodfl %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)    
spleglofl<-spleglofl[order(spleglofl$sp.name),]
colnames(splegfl)[2]<-"spnumfl"

load("Analyses/output/brms/testm5cent.brms.lofl.Rda")
#1 divergent transition
mod<-testm5cent.lodfl.brms
sum<-summary(mod)
fix<-sum$fixed[2:4,]
speff <- coef(mod)
rownames(fix)<-c("Temperature","Moisture","Temp*Mois")

pdf(file.path("Analyses/soilmoisture/figures/m5.lofl.samespp.pdf"), width = 10, height = 12)
#quartz(width = 10, height = 10)
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
  arrows(speff$sp[,"Q90",i+1],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"Q10",i+1],  5*(nrow(fix):1)[i]-.5-sp,
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


load("Analyses/output/brms/testm5cent.brms.fflo.Rda")

mod<-testm5cent.ffdlo.brms
sum<-summary(mod)
fix<-sum$fixed[2:4,]
speff <- coef(mod)
rownames(fix)<-c("Temperature","Moisture","Temp*Mois")

#minx<-min(speff$sp[,2:4,2:4])
#maxx<-max(speff$sp[,2:4,2:4])
#minx<--20
#maxx<-20
plot(seq(minx, #min(meanz[,'mean']*1.1),
         maxx, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix), length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of flowering",
     ylab = "",
     yaxt = "n")

axis(2, at = 5*(nrow(fix):1), labels = rownames(fix), las = 1, cex.axis = 0.8)

#i=1
#Plot species estimate for each predictor
sp<-4*(seq(1:dim(speff$sp)[1])/dim(speff$sp)[1])

# #colors differ by species
# nsp<-dim(speff$sp)[1]
# my.pal <- rep(brewer.pal(n = 12, name = "Set3"), 4)[1:nsp]
# my.pch <- rep(15:18, each=12)[1:nsp]

for(i in 1:nrow(fix)){
  arrows(speff$sp[,"Q90",i+1],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"Q10",i+1],  5*(nrow(fix):1)[i]-.5-sp,
         len = 0, col = alpha(my.pal, 0.5)) 
}
for(i in 1:nrow(fix)){
  points(speff$sp[,"Estimate",i+1], 5*(nrow(fix):1)[i]-.5-sp,
         pch = my.pch,
         col = alpha(my.pal, 0.5))
}
#fixed effects
arrows(fix[,"u-95% CI"], 5*(nrow(fix):1), fix[,"l-95% CI"], 5*(nrow(fix):1),
       len = 0, col = "purple3", lwd = 2)

points(fix[,'Estimate'],
       5*(nrow(fix):1),
       pch = 16,
       cex = 1.5,
       col = "purple3")

abline(v = 0, lty = 2)
par(xpd=TRUE) # so I can plot legend outside
leg1<-maxx+6
leg2<-5*(nrow(fix):1)[1]+5
legend(leg1, leg2, spleglofl$sp.name,
       pch=my.pch,
       col=alpha(my.pal, .5),
       bty = "n",
       cex=0.60, text.font=3)

dev.off()


#Plot all three versions of leaf out model together
pdf(file.path("Analyses/soilmoisture/figures/m5.alllomods.pdf"), width = 8, height = 12)
#quartz(width = 10, height = 10)
par(mfrow=c(3,1), mar = c(10, 10, 5, 10))

# First panel: leafout with all species
load("Analyses/output/brms/testm5cent.brms.lo.Rda")

mod<-testm5cent.lod.brms
sum<-summary(mod)
fix<-sum$fixed[2:4,]
speff <- coef(mod)
rownames(fix)<-c("Temperature","Moisture","Temp*Mois")

minx<-min(speff$sp[,2:4,2:4])
maxx<-max(speff$sp[,2:4,2:4])
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
  arrows(speff$sp[,"Q90",i+1],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"Q10",i+1],  5*(nrow(fix):1)[i]-.5-sp,
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
par(xpd=TRUE) # so I can plot legend outside
leg1<-maxx+6
leg2<-5*(nrow(fix):1)[1]+5
legend(leg1, leg2, splegbb$sp.name,
       pch=my.pch,
       col=alpha(my.pal, .5),
       bty = "n",
       cex=0.60, text.font=3)

#2nd panel: leafout from bb dataset
load("Analyses/output/brms/testm5cent.brms.lobb.Rda")

mod<-testm5cent.lodbb.brms
sum<-summary(mod)
fix<-sum$fixed[2:4,]
speff <- coef(mod)
rownames(fix)<-c("Temperature","Moisture","Temp*Mois")

# One panel: budburst

#minx<-min(speff$sp[,2:4,2:4])
#maxx<-max(speff$sp[,2:4,2:4])

plot(seq(minx, #min(meanz[,'mean']*1.1),
         maxx, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix), length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of leafout from bb dataset",
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
  arrows(speff$sp[,"Q90",i+1],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"Q10",i+1],  5*(nrow(fix):1)[i]-.5-sp,
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




par(xpd=TRUE) # so I can plot legend outside
leg1<-maxx+6
leg2<-5*(nrow(fix):1)[1]+5
legend(leg1, leg2, splegbblo$sp.name,
       pch=my.pch,
       col=alpha(my.pal, .5),
       bty = "n",
       cex=0.60, text.font=3)

#3rd panel, leafout with flowering dataset
#Now compare leaf out estimates fit to two leafot datasets vs this subset
mod<-testm5cent.lodfl.brms
sum<-summary(mod)
fix<-sum$fixed[2:4,]
speff <- coef(mod)
rownames(fix)<-c("Temperature","Moisture","Temp*Mois")

#minx<-min(speff$sp[,2:4,2:4])
#maxx<-max(speff$sp[,2:4,2:4])
#minx<--20
#maxx<-20
plot(seq(minx, #min(meanz[,'mean']*1.1),
         maxx, #max(meanz[,'mean']*1.1),
         length.out = nrow(fix)), 
     seq(1, 5*nrow(fix), length.out = nrow(fix)),
     type="n",
     xlab = "Model estimate, change in day of leafout from fl dataset",
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
  arrows(speff$sp[,"Q90",i+1],  5*(nrow(fix):1)[i]-.5-sp, speff$sp[,"Q10",i+1],  5*(nrow(fix):1)[i]-.5-sp,
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
par(xpd=TRUE) # so I can plot legend outside
leg1<-maxx+6
leg2<-5*(nrow(fix):1)[1]+5
legend(leg1, leg2, spleglofl$sp.name,
       pch=my.pch,
       col=alpha(my.pal, .5),
       bg=alpha(my.pal, .5),
       bty = "n",
       cex=0.60, text.font=3)


dev.off()


#Compare species level estimates in full models vs models with subset of sepcies
#For leafout:

full_lomod<-testm5cent.lod.brms
full_lotemp<-as.data.frame(cbind(rownames(coef(full_lomod)$sp[,,'temp']),coef(full_lomod)$sp[,,'temp']))
full_lomois<-as.data.frame(cbind(rownames(coef(full_lomod)$sp[,,'mois']), coef(full_lomod)$sp[,,'mois']))

full_loint<-as.data.frame(cbind(rownames(coef(full_lomod)$sp[,,'temp:mois']),coef(full_lomod)$sp[,,'temp:mois']))

colnames(full_lotemp)<-colnames(full_lomois)<-colnames(full_loint)<-c("sp","Est","Est.Er","Q10","Q90")

fl_lomod<-testm5cent.lodfl.brms
fl_lotemp<-as.data.frame(cbind(rownames(coef(fl_lomod)$sp[,,'temp']),coef(fl_lomod)$sp[,,'temp']))
fl_lomois<-as.data.frame(cbind(rownames(coef(fl_lomod)$sp[,,'mois']), coef(fl_lomod)$sp[,,'mois']))
fl_loint<-as.data.frame(cbind(rownames(coef(fl_lomod)$sp[,,'temp:mois']),coef(fl_lomod)$sp[,,'temp:mois']))
colnames(fl_lotemp)<-colnames(fl_lomois)<-colnames(fl_loint)<-c("sp","Est.fl","Est.Er.fl","Q10.fl","Q90.fl")

bb_lomod<-testm5cent.lodbb.brms
bb_lotemp<-as.data.frame(cbind(rownames(coef(bb_lomod)$sp[,,'temp']),coef(bb_lomod)$sp[,,'temp']))
bb_lomois<-as.data.frame(cbind(rownames(coef(bb_lomod)$sp[,,'mois']), coef(bb_lomod)$sp[,,'mois']))
bb_loint<-as.data.frame(cbind(rownames(coef(bb_lomod)$sp[,,'temp:mois']),coef(bb_lomod)$sp[,,'temp:mois']))
colnames(bb_lotemp)<-colnames(bb_lomois)<-colnames(bb_loint)<-c("sp","Est.bb","Est.Er.bb","Q10.bb","Q90.bb")

fullbb_lotemp<-left_join(bb_lotemp,full_lotemp, by = "sp")
fullbb_lomois<-left_join(bb_lomois,full_lomois)
fullbb_loint<-left_join(bb_loint,full_loint)
pdf(file.path("Analyses/soilmoisture/figures/compests_leafout.pdf"), width = 10, height = 12)
    
par(mfrow=c(1,3))
plot(fullbb_lotemp$Est,fullbb_lotemp$Est.bb, xlab = "Temp Effects (Full dataset)",ylab = "Temp Effects (BB species only",typ = "p", pch = 16)
lines(fullbb_lotemp$Est,fullbb_lotemp$Est)

plot(fullbb_lomois$Est,fullbb_lomois$Est.bb, xlab = "Mois Effects (Full dataset)",ylab = "Mois Effects (BB species only",typ = "p", pch = 16)
lines(fullbb_lomois$Est,fullbb_lomois$Est)

plot(fullbb_loint$Est,fullbb_loint$Est.bb, xlab = "Int Effects (Full dataset)",ylab = "Int Effects (BB species only",typ = "p", pch = 16)
lines(fullbb_loint$Est,fullbb_loint$Est)
dev.off()

  
)