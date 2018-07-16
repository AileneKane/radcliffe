#Started Sept 2017
#By Ailene

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

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#options(mc.cores = parallel::detectCores()) # added by Andrew
#update.packages()

# Setting working directory. Add in your own path in an if statement for your file structure
 if(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/radcliffe")}

#Goal: Fit a multimodel to phenology (budburst) data with temperature, soil moisture, and 
#their interaction as explanatory variables.
#
#Step 1: Fit the model with test data to make sure that the model can recover parameters accurately
###set up data 
n_sp=50#number of species
obs_sp=30#number of obs (plots, years) per species
N=n_sp*obs_sp#sample size
sp<-rep(seq(1:n_sp), each=obs_sp)#species ids

#set up distribution parameters
mu_a<-150#grand mean mean of bb doy
sigma_a<-5
mu_b_temp_sp<--2
sigma_b_temp_sp<-.1
mu_b_mois_sp<--1
sigma_b_mois_sp<-.1


a_sp<-as.integer(rnorm(n_sp,mu_a,sigma_a))#species specific day of year for bb

b_temp<-rnorm(n_sp,mu_b_temp_sp,sigma_b_temp_sp)#species specific effects of temp
b_mois<-rnorm(n_sp,mu_b_mois_sp,sigma_b_mois_sp)#species specific effects of mois

#create explanatory variables
#try using centered predictors
temp<-rep(NA, N)
for(i in 1:n_sp){
  temp[which(sp==i)]<-rnorm(obs_sp,0,1)#right now this is set up for each species but it doesn't need to be right?
}
mois<-rep(NA, N)
for(i in 1:n_sp){
  mois[which(sp==i)]<-rnorm(obs_sp,0,1)#we tried having the mean at ~25 for temp and mois, but rhat was bad and model had trouble... mean=5 was ok...
}


#model without interaction
sigma_y<-.5

#generate the response variable, ypred
ypred<-c()
for(i in 1:N){
  ypred[i] = a_sp[sp[i]] + b_temp[sp[i]] * temp[i] + b_mois[sp[i]] * mois[i]
}

y<-rnorm(N,ypred,sigma_y)
#check that test data look ok
plot(temp,y)
plot(mois,y)
hist(mois)
#try model in lmer
#chosen values:
#mu_a<-150#grand mean mean of bb doy
#sigma_a<-.5,
#mu_b_temp_sp<--2
#sigma_b_temp_sp<-.1
#mu_b_mois_sp<--1
#sigma_b_mois_sp<-.1

testm1.lmer<-lmer(y~temp + mois +(1|sp))
summary(testm1.lmer)#looks good!

#try the model with brms
testm1.brms <- brm(y ~ temp + mois +#fixed effects
                     (1|sp), #random effects
                   data=list(y=y,sp=sp,temp=temp, mois=mois, n_sp=n_sp,N=N),
                   chains = 2,iter = 2000,control = list(max_treedepth = 15))

stancode(testm1.brms)
summary(testm1.brms)
#brms model says a: 148.62, temp=-1.98, mois=-0.99, species sigma=0.52
marginal_effects(testm1.brms, surface = TRUE)


#now fit the model in stan for comparison- haven't done this in a while...
testm1 = stan('Analyses/soilmoisture/M1_bbd_testdata.stan', data=list(y=y,sp=sp,temp=temp, mois=mois, n_sp=n_sp,N=N),
              iter = 2500, warmup=1500) # 
beta_draws<-as.matrix(testm1,pars=c("b_temp","b_mois","sigma_y"))
mcmc_intervals(beta_draws)
head(summary(testm1)$summary)
summary(testm1)$summary
launch_shinystan(testm1)#this can be slow

#M2: model with interaction
mu_b_tm_sp<--.1
sigma_b_tm_sp<-.005
b_tm<-rnorm(n_sp,mu_b_tm_sp,sigma_b_tm_sp)#species specific interaction

ypred<-c()
for(i in 1:N){
  ypred[i] = a_sp[sp[i]] + b_temp[sp[i]] * temp[i] + b_mois[sp[i]] * mois[i]+ b_tm[sp[i]]*temp[i] * mois[i]
}

y<-rnorm(N,ypred,sigma_y)
#check that test data look ok
plot(temp,y)
plot(mois,y)
hist(mois)
#try model in lmer
testm2.lmer<-lmer(y~temp * mois +(1|sp))#
summary(testm2.lmer)
#brms

#try the model with brms
testm2.brms <- brm(y ~ temp * mois +#fixed effects
                     (1|sp), #random effects
                   data=list(y=y,sp=sp,temp=temp, mois=mois, n_sp=n_sp,N=N),
                   chains = 2,control = list(max_treedepth = 15)) 

stancode(testm2.brms)
summary(testm2.brms)#looks good!
#brms model says a: 148.82, temp=-2.01, mois=-1.00, tmint=-0.09, species sigma=0.52
marginal_effects(testm2.brms, surface = TRUE)


#now fit the model in stan
testm2 = stan('Analyses/soilmoisture/M2_bbd_testdata.stan', data=list(y=y,sp=sp,temp=temp, mois=mois,n_sp=n_sp,N=N),
              iter = 2500) # , warmup=1500, get warning about maximum treedepth when, , control=list(max_treedepth=15)

beta_draws<-as.matrix(testm2,pars=c("b_temp","b_mois","b_tm","sigma_y"))
mcmc_intervals(beta_draws)
head(summary(testm2)$summary)
launch_shinystan(testm2)#t
#the above models work fine with centered data...but when mean of temp/mois=25, it has trouble. not sure why! using centered data for now

#M3: model with interaction and varying slopes for species

#try model in lmer
testm3.lmer<-lmer(y~temp * mois +(temp*mois|sp))#
summary(testm3.lmer)#fixed effects look pretty good

#try the model with brms
testm3.brms <- brm(y ~ temp * mois +#fixed effects
                     (temp * mois|sp), #random effects
                   data=list(y=y,sp=sp,temp=temp, mois=mois, n_sp=n_sp,N=N),
                   chains = 2,control = list(max_treedepth = 15)) 

stancode(testm3.brms)
summary(testm3.brms)
#brms model says a: 148.85, temp=-2.01, mois=-1.00, tmint=-0.09, species sigma=0.50
marginal_effects(testm3.brms, surface = TRUE)
#looks good!!!

#now fit the model in stan- haven't done this in a while...
testm3 = stan('Analyses/soilmoisture/M3_bbd_testdata.stan', data=list(y=y,sp=sp,temp=temp, mois=mois,n_sp=n_sp,N=N),
              iter = 5000, warmup=3500,control=list(adapt_delta=.95)) # there were divergent transitions. looked at bivariate plots, and problem parameter seems to be sigma_tm. sigma_temp also was a bit weird
#Try making noncentered paraterization 

testm3_ncp = stan('Analyses/soilmoisture/M3_bbd_testdata_ncp.stan', data=list(y=y,sp=sp,temp=temp, mois=mois,n_sp=n_sp,N=N),
              iter = 4000) 

beta_draws<-as.matrix(testm3_ncp,pars=c("b_temp","b_mois","b_tm","sigma_y"))
mcmc_intervals(beta_draws)
head(summary(testm3_ncp)$summary)

launch_shinystan(testm3_ncp)#this can be slow


#M4: With site added as intercept only random effect
n_site=10#number of sites
obs_site=N/n_site#number of obs (plots, years) per site (N is defined above)
sigma_a_site<-3
a_site<-as.integer(rnorm(n_site,mu_a,sigma_a_site))#site specific day of year for bb
#is this right? use grand mean again (as for species? just variance is different)
#not sure if more variance among site or species makes more sense...i made site-level variance smaller for now

site<-rep(seq(1:n_site), each=obs_site)#site ids

ypred<-c()
for(i in 1:N){
  ypred[i] = a_site[site[i]] + a_sp[sp[i]] + b_temp[sp[i]] * temp[i] + b_mois[sp[i]] * mois[i]+ b_tm[sp[i]]*temp[i] * mois[i]
}

y<-rnorm(N,ypred,sigma_y)
#check that test data look ok
plot(temp,y)
plot(mois,y)
hist(mois)

testm4.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site))#converged when site variance =3
summary(testm4.lmer)#fixed effects look pretty good
#try the model with brms
testm4.brms <- brm(y ~ temp * mois +#fixed effects
                     (temp * mois|sp) + (1|site), #random effects
                   data=list(y=y,sp=sp,temp=temp, mois=mois, n_sp=n_sp,N=N, site=site),
                   chains = 2,control = list(max_treedepth = 15,adapt_delta = 0.99)) 
 #the above code is really, really slow....
stancode(testm4.brms)
summary(testm4.brms)
#brms model says a: 299.44, temp=-1.97, mois=-1.03, tmint=-0.11, species sigma=0.51
marginal_effects(testm4.brms, surface = TRUE)
#looks good!!! but took a really long time to fit...

#testm4 = stan('Analyses/soilmoisture/M4_bbd_testdata.stan', data=list(y=y,sp=sp,site=site,temp=temp, mois=mois,n_sp=n_sp,n_site=n_site,N=N)) 
#Warning messages:
#  1: There were 3999 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
#http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
#2: There were 4 chains where the estimated Bayesian Fraction of Missing Information was low. See
#http://mc-stan.org/misc/warnings.html#bfmi-low 
#3: Examine the pairs() plot to diagnose sampling problems
#beta_draws<-as.matrix(testm4,pars=c("b_temp","b_mois","b_tm","sigma_y","sigma_a_sp","sigma_a_site"))
#mcmc_intervals(beta_draws)
#head(summary(testm4)$summary)

#launch_shinystan(testm4)#this can be slo

#testm4_ncp = stan('Analyses/soilmoisture/M4_bbd_testdata_ncp.stan', data=list(y=y,sp=sp,site=site,temp=temp, mois=mois,n_sp=n_sp,n_site=n_site,N=N),
#                  iter = 4000,control=list(max_treedepth=15)) # I get warning about maximum treedepth 

#beta_draws<-as.matrix(testm4_ncp,pars=c("b_temp","b_mois","b_tm","sigma_y","sigma_a_sp","sigma_a_site"))
#mcmc_intervals(beta_draws)
#head(summary(testm4_ncp)$summary)

#launch_shinystan(testm4_ncp)#this can be slow

###Now with the data
###try without ncp if treedepth issue doesn't solve things.

#source('Analyses/soilmoisture/savestan.R')

#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())

#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)

#standard data wrangling to get expclim2 for climate analyses and expgdd for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")

#summarize climate data by plot (annual and seasonal temp, soil mois), 
  #merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
source("Analyses/soilmoisture/climsum_byplot.R")

#Prep the data for Stan model
expgdd_subs$genus.species<-as.numeric(as.factor(expgdd_subs$genus.species))
expgdd_subs$site<-as.numeric(as.factor(expgdd_subs$site))
expgdd_subs$year<-as.numeric(as.factor(expgdd_subs$year))


#1) Divide by phenophase:

expgdd_bbd<-expgdd_subs[which(expgdd_subs$event=="bbd"),]#bud burst data
expgdd_bbd <- expgdd_bbd[apply(expgdd_bbd, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

expgdd_lod<-expgdd_subs[which(expgdd_subs$event=="lod"),]#leaf out data
expgdd_lod <- expgdd_lod[apply(expgdd_lod, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

expgdd_lud<-expgdd_subs[which(expgdd_subs$event=="lud"),]#leaf unfolding data
expgdd_lud <- expgdd_lud[apply(expgdd_lud, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

expgdd_ffd<-expgdd_subs[which(expgdd_subs$event=="ffd"),]#leaf unfolding data
expgdd_ffd <- expgdd_ffd[apply(expgdd_ffd, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

expgdd_ffrd<-expgdd_subs[which(expgdd_subs$event=="ffrd"),]#leaf unfolding data
expgdd_ffrd <- expgdd_ffrd[apply(expgdd_ffrd, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

expgdd_sen<-expgdd_subs[which(expgdd_subs$event=="sen"),]#leaf unfolding data
expgdd_sen <- expgdd_sen[apply(expgdd_sen, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

#For lod and lud, use only species which have all lod and lud
unique(expgdd_lud$genus.species)#many fewer species have lud- do not sure this one!
unique(expgdd_lod$genus.species)
common.spp<-unique(expgdd_lud$genus.species[expgdd_lud$genus.species%in%expgdd_lod$genus.species])
unique(expgdd_sen$genus.species)
expgdd_lod_cs<-expgdd_lod[which(expgdd_lod$genus.species%in%common.spp),]
expgdd_lud_cs<-expgdd_lud[which(expgdd_lud$genus.species%in%common.spp),]

# For centering data:
expgdd_bbd$sm_cent <- scale(expgdd_bbd$sm, center=TRUE, scale=TRUE)
expgdd_bbd$smjm_cent<-scale(expgdd_bbd$soilmois_janmar, center = TRUE, scale = TRUE)
expgdd_bbd$ag_min_jm_cent<-scale(expgdd_bbd$ag_min_janmar, center = TRUE, scale = TRUE)
expgdd_bbd$agtmax_cent<-scale(expgdd_bbd$agtmax, center = TRUE, scale = TRUE)

expgdd_lod$sm_cent <- scale(expgdd_lod$sm, center=TRUE, scale=TRUE)
expgdd_lod$smjm_cent<-scale(expgdd_lod$soilmois_janmar, center = TRUE, scale = TRUE)
expgdd_lod$ag_min_jm_cent<-scale(expgdd_lod$ag_min_janmar, center = TRUE, scale = TRUE)
expgdd_lod$agtmax_cent<-scale(expgdd_lod$agtmax, center = TRUE, scale = TRUE)
expgdd_lod$ag_min_aprjun_cent<-scale(expgdd_lod$ag_min_aprjun, center = TRUE, scale = TRUE)
expgdd_lod$soilmois_aprjun_cent<-scale(expgdd_lod$soilmois_aprjun, center = TRUE, scale = TRUE)

expgdd_lud$sm_cent <- scale(expgdd_lud$sm, center=TRUE, scale=TRUE)
expgdd_lud$smjm_cent<-scale(expgdd_lud$soilmois_janmar, center = TRUE, scale = TRUE)
expgdd_lud$ag_min_jm_cent<-scale(expgdd_lud$ag_min_janmar, center = TRUE, scale = TRUE)
expgdd_lud$agtmax_cent<-scale(expgdd_lud$agtmax, center = TRUE, scale = TRUE)

expgdd_ffd$sm_cent <- scale(expgdd_ffd$sm, center=TRUE, scale=TRUE)
expgdd_ffd$agtmin_cent<-scale(expgdd_ffd$agtmin, center = TRUE, scale = TRUE)
expgdd_ffd$agtmax_cent<-scale(expgdd_ffd$agtmax, center = TRUE, scale = TRUE)

expgdd_ffrd$sm_cent <- scale(expgdd_ffrd$sm, center=TRUE, scale=TRUE)
expgdd_ffrd$agtmin_cent<-scale(expgdd_ffrd$agtmin, center = TRUE, scale = TRUE)
expgdd_ffrd$agtmax_cent<-scale(expgdd_ffrd$agtmax, center = TRUE, scale = TRUE)

expgdd_sen$sm_cent <- scale(expgdd_sen$sm, center=TRUE, scale=TRUE)
expgdd_sen$agtmin_cent<-scale(expgdd_sen$agtmin, center = TRUE, scale = TRUE)
expgdd_sen$agtmax_cent<-scale(expgdd_sen$agtmax, center = TRUE, scale = TRUE)

#2) Make a list out of the processed data. It will be input for the model.
datalist.bbd <- with(expgdd_bbd, 
                    list(y = doy, 
                         temp = ag_min_janmar, #above-ground minimum air temp
                         mois = soilmois_janmar, #soil moisture
                         sp = genus.species,
                         site = site,
                         N = nrow(expgdd_bbd),
                         n_sp = length(unique(expgdd_bbd$genus.species))
                    )
)

datalist.bbd.cent <- with(expgdd_bbd, 
                          list(y = doy, 
                               temp = ag_min_jm_cent[,1], #above-ground minimum air temp
                               mois = smjm_cent[,1], #soil moisture
                               sp = genus.species,
                               site = site,
                               N = nrow(expgdd_bbd),
                               n_sp = length(unique(expgdd_bbd$genus.species))
                          )
)

##################
# Fit m4 to data #
##################
testm4.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site),data=datalist.bbd)#converged when site variance =3
summary(testm4.lmer)#model fits! a=112.694; temp= -4.048; mois=-78.903; temp:mois=2.463, site var=34.08; sp

testm4cent.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site),data=datalist.bbd.cent)#converged when site variance =3
summary(testm4cent.lmer)#model fits! temp= -15.01; mois=-3.32; temp:mois=-0.33

#try the model with brms
testm4.brms <- brm(y ~ temp * mois +#fixed effects
                     (temp * mois|sp) + (1|site), #random effects
                   data=datalist.bbd,
                   chains = 2,
                   control = list(max_treedepth = 15,adapt_delta = 0.99))# 


stancode(testm4.brms)
summary(testm4.brms)
#brms model says a: 299.44, temp=-1.97, mois=-1.03, tmint=-0.11, species sigma=0.51
#brms model with site says a: 93.01, temp=-14.49, mois=-2.62 , tmint=- 0.01 , species sigma=0.51

marginal_effects(testm4.brms, surface = TRUE)
#2 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. but took a really long time to fit...7692.48 seconds (=2.1368 hrs per chain)
stanplot(testm4.brms)
ranef(testm4.brms)
stanplot(testm4.brms, pars = "^b_")
#       point_size = 3, r_col = "black", r_intervals = FALSE, r_alpha = 0.5)


##################
# Fit m5 to data #
##################
datalist.bbd <- with(expgdd_bbd, 
                     list(y = doy, 
                          temp = ag_min_janmar, #above-ground minimum air temp
                          mois = soilmois_janmar, #soil moisture
                          sp = genus.species,
                          site = site,
                          year = year,
                          N = nrow(expgdd_bbd),
                          n_sp = length(unique(expgdd_bbd$genus.species))
                     )
)

datalist.bbd.cent <- with(expgdd_bbd, 
                          list(y = doy, 
                               temp = ag_min_jm_cent[,1], #above-ground minimum air temp
                               mois = smjm_cent[,1], #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_bbd),
                               n_sp = length(unique(expgdd_bbd$genus.species))
                          )
)

testm5.lmer<-lmer(y~temp * mois +
                    (temp*mois|sp)+ (1|site/year),
                  data=datalist.bbd)
summary(testm5.lmer)
#model fits! a=108.961,temp= -3.734; mois=--33.070; temp:mois= 2.954

testm5cent.lmer<-lmer(y~temp * mois +
                      (temp*mois|sp)+ (1|site/year),
                      data=datalist.bbd.cent)
summary(testm5cent.lmer)
#model fits! a=99.4208, temp= -10.1869; mois=--1.2633; temp:mois=-0.3990

#try the model with brms
testm5.brms <- brm(y ~ temp * mois +#fixed effects
                     (temp * mois|sp) + (1|site/year), #random effects
                   data=datalist.bbd,
                   chains = 2)# control = list(max_treedepth = 15,adapt_delta = 0.99)

stancode(testm5.brms)#has 31 divergent transitions
summary(testm5.brms)
stanplot(testm5.brms, pars = "^b_")

stanplot(testm5.brms, surface = TRUE)
#2 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. 

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

#LUD and LOD in lmer
mean(expgdd_bbd$doy, na.rm=TRUE)#99.95649
mean(expgdd_lud$doy, na.rm=TRUE)#106.
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

datalist.lud.cent <- with(expgdd_lud, 
                          list(y = doy, 
                               temp = ag_min_jm_cent, #above-ground minimum air temp
                               mois = smjm_cent, #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_bbd),
                               n_sp = length(unique(expgdd_bbd$genus.species))
                          )
)

datalist.lud <- with(expgdd_lud, 
                          list(y = doy, 
                               temp = ag_min_janmar, #above-ground minimum air temp
                               mois = soilmois_janmar, #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_bbd),
                               n_sp = length(unique(expgdd_bbd$genus.species))
                          )
)

lud.testm5.lmer<-lmer(y~temp * mois +
                        (temp*mois|sp)+ (1|site/year),
                      data=datalist.lud)#
summary(lud.testm5.lmer)#model fits! 
#temp= -7.893; mois=7.655; temp:mois= 18.238
ludcent.testm5.lmer<-lmer(y~temp * mois +
                        (temp*mois|sp)+ (1|site/year),
                      data=datalist.lud.cent)#converged when site variance =3
summary(ludcent.testm5.lmer)#model fits! 
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
ffd.testm5.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site/year),data=datalist.ffd.cent)#converged when site variance =3
summary(ffd.testm5.lmer)#model fits! 
#temp= -7.1193; mois=-2.4218 ; temp:mois= -2.4283

ffrd.testm5.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site/year),data=datalist.ffrd.cent)#converged when site variance =3
summary(ffrd.testm5.lmer)#model fits! 
#temp= -5.1190; mois=-0.3049 ; temp:mois= -0.7659 

sen.testm5.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site/year),data=datalist.sen.cent)#converged when site variance =3
summary(sen.testm5.lmer)#model fits! 
#temp= 1.311 ; mois=-8.317 ; temp:mois= -4.695

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
