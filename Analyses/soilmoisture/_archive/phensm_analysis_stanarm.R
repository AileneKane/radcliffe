## Started in 2016 by D Flynn ##
## Lizzie started working on it in Jan 2018 ##

# Budburst Chilling Experiment 2016 analysis
rm(list=ls())

# library(nlme) # switching to rstanarm models by Lizzie in Jan 2018
library(scales)
library(arm)
library(rstan)
library(shinystan)
# library(sjPlot)
library(rstanarm)
library(RColorBrewer)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

runstan = FALSE

# setwd("~/Documents/git/budchill/analyses")
if(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/radcliffe")}
source('stan/savestan.R')
#print(toload <- sort(dir("./input")[grep("Budburst Chill Data", dir('./input'))], T)[1])

#load(file.path("input", toload))

# Initial analysis: by experimental treatment
# convert chill and time to numerics for ordered analysis
#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/expclim.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)

#standard data wrangling to get expclim2 for climate analyses and  expgdd for phenology analyses (with gddcrit)
source("analyses/source/standard_mergesandwrangling.R")

#summarize climate data by plot (annual and seasonal temp, soil mois) and merge in with expgdd file
source("analyses/soilmoisture/climsum_byplot.R")

expclim2$target_cent<-scale(expclim2$target, center = TRUE, scale = TRUE)
expclim2$preciptreat_amt_cent<-scale(expclim2$preciptreat_amt, center = TRUE, scale = TRUE)
expclim2a<-subset(expclim2,select=c(site,year,doy,target_cent,preciptreat_amt,target,preciptreat_amt_cent,soilmois1))
expclim2a<- expclim2a [apply(expclim2a , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
table(expclim2a$site,expclim2a$year)
#First, 
#1) How do warming and precip treatments affect soil moisture? (Make plots and fit models)


#Prep the data for Stan model
expclim2a$site<-as.numeric(as.factor(expclim2a$site))
expclim2a$year<-as.numeric(as.factor(expclim2a$year))

#2) Make a list out of the processed data. It will be input for the model.
datalist.sm <- with(expclim2a, 
                    list(y = soilmois1, 
                         target = target, #target warming treatment
                         prec = preciptreat_amt*100, #precip treatment (percentage)
                         site = site,
                         year=year,
                         doy=doy,
                         N = nrow(expclim2a),
                         n_site = length(unique(expclim2a$site))
                    )
)

datalist.sm.cent <- with(expclim2a, 
                         list(y = soilmois1, 
                              target = target_cent, #target warming treatment
                              prec = preciptreat_amt_cent, #precip treatment (percentage)
                              site = site,
                              year=year,
                              doy=doy,
                              N = nrow(expclim2a),
                              n_site = length(unique(expclim2a$site))
                         )
)


if(runstan){
m1 <- stan_lmer(bday ~ chilltemp * timetreat + (1|sp), data = dx)
summary(m1)

m1.all <- stan_lmer(bday ~ (chill*time) +(chill*time|sp), data = dx)
summary(m1.all)
save(m1.all, file="output/m1.allbb.Rdata")

}

if(!runstan){
load("output/m1.allbb.Rdata")
}

sumer.m1.all <- summary(m1.all)
iter.m1.all <- as.data.frame(m1.all)

# manually to get right order, with intercept
params <- c("chillchill2", "chillchill4", "chillchill8","timetime2", "timetime3",
    "chillchill2:timetime2", "chillchill4:timetime2", "chillchill8:timetime2", 
    "chillchill2:timetime3", "chillchill4:timetime3", "chillchill8:timetime3")

col4fig <- c("mean","sd","25%","50%","75%","Rhat")

meanzb.wi <- sumer.m1.all[params,col4fig]

rownames(meanzb.wi) = c("Chilling at 2°C",
                    "Chilling at 4°C",
                    "Chilling at 8°C",
                    "16 days additional chilling",
                    "32 days additional chilling",
                    "16 days x chilling 2°C",
                    "16 days x chilling 4°C",
                    "16 days x chilling 8°C",
                    "32 days x chilling 2°C",
                    "32 days x chilling 4°C",
                    "32 days x chilling 8°C"
                    )

my.pal <- brewer.pal(n = 8, name = "Dark2")

speff.bb <- speff.lo <- vector()

pdf(file.path("figures/m1.all.pdf"), width = 7, height = 8)

par(mfrow=c(1,1), mar = c(6, 10, 2, 1))
# One panel: budburst
plot(seq(-15, #min(meanz[,'mean']*1.1),
         10, #max(meanz[,'mean']*1.1),
         length.out = nrow(meanzb.wi)), 
     seq(1, 5*nrow(meanzb.wi), length.out = nrow(meanzb.wi)),
     type="n",
     xlab = "Model estimate change in day of budburst",
     ylab = "",
     yaxt = "n")

# legend(x =-16.5, y = 2, bty="n", legend = "Budburst", text.font = 2)
# rasterImage(bbpng, -0.25, 1, 0, 4)

axis(2, at = 5*(nrow(meanzb.wi):1), labels = rownames(meanzb.wi), las = 1, cex.axis = 0.8)


# Plot species levels for each predictor
for(i in 1:length(unique(dx$sp))){
  b.params <- iter.m1.all[!is.na(match(colnames(iter.m1.all), c(paste("b", "[", params, " sp:",
      unique(dx$sp)[i], "]", sep=""))))]

  main.params <- iter.m1.all[!is.na(match(colnames(iter.m1.all), params))]

  bplusmain <- b.params
  for(c in 1:ncol(main.params)){
      bplusmain[c] <- b.params[c]+main.params[c]
      }

  bplusmain.quant <- sapply(bplusmain, FUN = quantile, probs = c(0.25, 0.50, 0.75))
  
  sp.est <- t(bplusmain.quant)
  
  jt <- jitter(0, factor = 40)

  arrows(sp.est[,"75%"],  jt+(5*(nrow(meanzb.wi):1)-1), sp.est[,"25%"],  jt+(5*(nrow(meanzb.wi):1)-1),
         len = 0, col = alpha(my.pal[i], 0.2)) 
  
  points(sp.est[,'50%'],
         jt+(5*(nrow(meanzb.wi):1)-1), #[c(3:5,11:12)], # ADJUSTED for just the ranef here
         pch = 16,
         col = alpha(my.pal[i], 0.5))

  speff.bb = rbind(speff.bb, t(sp.est[,1]))
    }

arrows(meanzb.wi[,"75%"], (5*(nrow(meanzb.wi):1))+0.25, meanzb.wi[,"25%"], (5*(nrow(meanzb.wi):1))+0.25,
       len = 0, col = "black", lwd = 3)

points(meanzb.wi[,'mean'],
       (5*(nrow(meanzb.wi):1))+0.25,
       pch = 16,
       cex = 1,
       col = "midnightblue")
abline(v = 0, lty = 2)

legend("topleft", legend=unique(dx$sp), col=alpha(my.pal[1:7], 0.5), pch=16, bty="n", cex=0.7)
dev.off()




########### No chill and 1 and 4 C only ###########
dx.14 <- subset(dx, chill=="chill1" | chill=="chill4")

## Recoding by Lizzie
# If none of the chilling happened until 1 January, then the chilltemp coding is INCORRECT, because there is no chilltemp to experience in time1 #
## One way to fix this (shown below) results in the same model estimates as best I can tell. 
if(FALSE){
dx.14$chilltemp[dx.14$timetreat=="1"] <- 0
dx.14$chilltemp <- as.factor(dx.14$chilltemp)
dx.14$timetreat <- as.factor(dx.14$timetreat)
}
# Note that my old code where I tried to overcode the factor ended up in overcoding too much -- be careful with factors....

if(runstan){
m1.14 <- stan_lmer(bday ~ (chill*time) +(chill*time|sp), data = dx.14) # warnings.
save(m1.14, file="output/m1.14bb.Rdata")
}
# 1 day advance due to 4 degree temperature (compared to 1 C), 8 and 10 day advance due to longer time ...

if(!runstan){
load("output/m1.14bb.Rdata")
}
#
sumer.m1.14 <- summary(m1.14)
iter.m1.14 <- as.data.frame(m1.14)

# manually to get right order, with intercept
params <- c("chillchill4","timetime2","timetime3",
               "chillchill4:timetime2","chillchill4:timetime3")

col4fig <- c("mean","sd","25%","50%","75%","Rhat")

meanzb.wi <- sumer.m1.14[params,col4fig]

rownames(meanzb.wi) = c("Chilling at 4°C",
                    "16 days additional chilling",
                    "32 days additional chilling",
                    "16 days x chilling 4°C",
                    "32 days x chilling 4°C"
                    )

speff.bb <- speff.lo <- vector()

pdf(file.path("figures/m1and14.pdf"), width = 7, height = 6)

par(mfrow=c(1,1), mar = c(6, 10, 2, 1))
# One panel: budburst
plot(seq(-15, #min(meanz[,'mean']*1.1),
         10, #max(meanz[,'mean']*1.1),
         length.out = nrow(meanzb.wi)), 
     seq(1, 5*nrow(meanzb.wi), length.out = nrow(meanzb.wi)),
     type="n",
     xlab = "Model estimate change in day of budburst",
     ylab = "",
     yaxt = "n")

# legend(x =-16.5, y = 2, bty="n", legend = "Budburst", text.font = 2)
# rasterImage(bbpng, -0.25, 1, 0, 4)

axis(2, at = 5*(nrow(meanzb.wi):1), labels = rownames(meanzb.wi), las = 1, cex.axis = 0.8)


# Plot species levels for each predictor
for(i in 1:length(unique(dx.14$sp))){
  b.params <- iter.m1.14[!is.na(match(colnames(iter.m1.14), c(paste("b", "[", params, " sp:",
      unique(dx$sp)[i], "]", sep=""))))]

  main.params <- iter.m1.14[!is.na(match(colnames(iter.m1.14), params))]

  bplusmain <- b.params
  for(c in 1:ncol(main.params)){
      bplusmain[c] <- b.params[c]+main.params[c]
      }

  bplusmain.quant <- sapply(bplusmain, FUN = quantile, probs = c(0.25, 0.50, 0.75))
  
  sp.est <- t(bplusmain.quant)
  
  jt <- jitter(0, factor = 40)

  arrows(sp.est[,"75%"],  jt+(5*(nrow(meanzb.wi):1)-1), sp.est[,"25%"],  jt+(5*(nrow(meanzb.wi):1)-1),
         len = 0, col = alpha("firebrick", 0.2)) 
  
  points(sp.est[,'50%'],
         jt+(5*(nrow(meanzb.wi):1)-1), #[c(3:5,11:12)], # ADJUSTED for just the ranef here
         pch = 16,
         col = alpha("firebrick", 0.5))

  speff.bb = rbind(speff.bb, t(sp.est[,1]))
    }

arrows(meanzb.wi[,"75%"], (5*(nrow(meanzb.wi):1))+0.25, meanzb.wi[,"25%"], (5*(nrow(meanzb.wi):1))+0.25,
       len = 0, col = "black", lwd = 3)

points(meanzb.wi[,'mean'],
       (5*(nrow(meanzb.wi):1))+0.25,
       pch = 16,
       cex = 1,
       col = "midnightblue")
abline(v = 0, lty = 2)
dev.off()

########### USE THIS (Dan's analyses) #################
keepsp <- table(dx$nl, dx$sp)[2,] / table(dx$sp) >= 0.25 # all

if(runstan){
m2 <- stan_lmer(bday ~ (chilltemp*timetreat|sp/ind), data = dx[dx$sp %in% names(keepsp)[keepsp==TRUE],] ) # note that this one does not give overall effects across species!
summary(m2)
ranef(m2)

m2l <- stan_lmer(lday ~ (chilltemp*timetreat|sp), data = dx)
summary(m2l)
save(m2l, file="output/m2l.RData")
# load("output/m2l.RData")
}

# temperature effects non linear?
summary.aov(lm(bday ~ chilltemp * timetreat * sp, data = dx))
plot(bday ~ chilltemp * sp, data = dx)

means <- aggregate(dx$bday, list(chill=dx$chilltemp, time=dx$timetreat, sp=dx$sp), mean, na.rm=T)
ses <- aggregate(dx$bday, list(dx$chilltemp, dx$timetreat, dx$sp), function(x) sd(x,na.rm=T)/sqrt(length(x[!is.na(x)])))
datx <- data.frame(means, se=ses$x)

pdf(file="figures/budburst_bytimechill.pdf", width=14, height=6)
ggplot(datx, aes(time, x, group = as.factor(chill), color=chill)) + geom_line(lwd = 2) +
    geom_errorbar(aes(x=time, ymin=x-se, ymax=x+se), width=0) +
    facet_grid(.~sp) + ylab('Day of budburst') +
    xlab('Chilling time') +
    scale_colour_gradient(low="red4", high = "lemonchiffon") # high = "#56B1F7"
dev.off()

# subset down to just 1 and 4 C
datx14 <- subset(datx, chill==1|chill==4)

pdf(file="figures/budburst_bytimechill_1Cand4C.pdf", width=14, height=6)
ggplot(datx14, aes(time, x, group = as.factor(chill), color=chill)) + geom_line(lwd = 2) +
    geom_errorbar(aes(x=time, ymin=x-se, ymax=x+se), width=0) +
    facet_grid(.~sp) + ylab('Day of budburst') +
    xlab('Chilling time') +
    scale_colour_gradient(low="red4", high = "#56B1F7") 
dev.off()

m3 <- lmer(bday ~ chillport + chilltemp * timetreat + (1|sp), data = dx)
summary(m3)
sjp.lmer(m3, type = 'fe')

# Are chill portions better predictors of budburst than temperature?
m4 <- lmer(bday ~ chillport * chilltemp * timetreat + (1|sp), data = dx)
summary(m4)
sjp.lmer(m3, type = 'fe')

AIC(m1,m3,m4)

# no, calculated chill portions at least by AIC are not as good as chill temp timetreat
m5 <- lmer(bday ~ chillport  + (1|sp/ind), data = dx)
m6 <- lmer(bday ~ chilltemp  * timetreat + (1|sp/ind), data = dx)

AIC(m5, m6)

summary(m5)
summary(m6)

# check for leafout. Now not so different, but not better
m5 <- lmer(lday ~ chillport  + (1|sp/ind), data = dx)
m6 <- lmer(lday ~ chilltemp  * timetreat + (1|sp/ind), data = dx)

AIC(m5, m6)

summary(m5)
summary(m6)

######### Stan.

# make dummy vars. Can we do 4 levels of chill treatment without a 'reference' level? More in the spirit of Bayesian..
# now doing with chill1 as reference.
dx$chill1 = ifelse(dx$chill == "chill1", 1, 0) 
dx$chill2 = ifelse(dx$chill == "chill2", 1, 0) 
dx$chill4 = ifelse(dx$chill == "chill4", 1, 0) 
dx$chill8 = ifelse(dx$chill == "chill8", 1, 0) 
dx$time2 = ifelse(dx$time == "time2", 1, 0) 
dx$time3 = ifelse(dx$time == "time3", 1, 0) 

with(dx, table(time2, time3))

with(dx, table(chill1, chill2))
with(dx, table(chill4, chill8))

dxb <- dx[!is.na(dx$bday),] # ignore those which failed to burst bud

datalist.b <- list(lday = dxb$bday, # budburst as respose 
                  sp = as.numeric(dxb$sp), 
                  chill1 = as.numeric(dxb$chill1),
                  chill2 = as.numeric(dxb$chill2),
                  chill4 = as.numeric(dxb$chill4),
                  chill8 = as.numeric(dxb$chill8),
                  time2 = as.numeric(dxb$time2),
                  time3 = as.numeric(dxb$time3),
                   N = nrow(dxb), 
                   n_sp = length(unique(dxb$sp))
)

  doym.b <- stan('stan/chill_time_sp1.stan', 
                 data = datalist.b, iter = 5005, chains = 4,
                 control = list(adapt_delta = 0.9,
                                max_treedepth = 15))

# 266 divergences with all 4 levels, still 128 divergences with 3-level version
  sumerb <- summary(doym.b)$summary
  sumerb[grep("mu_", rownames(sumerb)),]
  
  ssm.b <- as.shinystan(doym.b)
  # launch_shinystan(ssm.b) 
savestan("Chill1 bud")
