#Started Sept 2017
#By Ailene

#Use Stan to fit soil moisture- phenology model to radcliffe data 

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
sigma_a_site<-5
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

testm4.lmer<-lmer(y~temp * mois +(temp*mois|sp)+ (1|site))#failed to converge
summary(testm4.lmer)#but fixed effects look pretty good
#try the model with brms
testm4.brms <- brm(y ~ temp * mois +#fixed effects
                     (temp * mois|sp) + (1|site), #random effects
                   data=list(y=y,sp=sp,temp=temp, mois=mois, n_sp=n_sp,N=N, site=site),
                   chains = 2,control = list(max_treedepth = 15,adapt_delta = 0.99)) 
#the above code is really slow....
stancode(testm4.brms)
summary(testm4.brms)
#brms model says a: 148.85, temp=-2.01, mois=-1.00, tmint=-0.09, species sigma=0.50
marginal_effects(testm4.brms, surface = TRUE)
#looks good!!!

testm4 = stan('Analyses/soilmoisture/M4_bbd_testdata.stan', data=list(y=y,sp=sp,site=site,temp=temp, mois=mois,n_sp=n_sp,n_site=n_site,N=N)) 
#Warning messages:
#  1: There were 3999 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
#http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
#2: There were 4 chains where the estimated Bayesian Fraction of Missing Information was low. See
#http://mc-stan.org/misc/warnings.html#bfmi-low 
#3: Examine the pairs() plot to diagnose sampling problems
beta_draws<-as.matrix(testm4,pars=c("b_temp","b_mois","b_tm","sigma_y","sigma_a_sp","sigma_a_site"))
mcmc_intervals(beta_draws)
head(summary(testm4)$summary)

launch_shinystan(testm4)#this can be slo

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

#1) Divide by phenophase:

expgdd_bbd<-expgdd_subs[which(expgdd_subs$event=="bbd"),]#bud burst data
expgdd_bbd <- expgdd_bbd[apply(expgdd_bbd, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
expgdd_bbd$genus.species<-as.numeric(as.factor(expgdd_bbd$genus.species))

expgdd_lod<-expgdd_subs[which(expgdd_subs$event=="lod"),]#leaf out data

expgdd_lud<-expgdd_subs[which(expgdd_subs$event=="lud"),]#leaf unfolding data

## For centering data, not doing it for now:
expgdd_bbd$sm_cent <- scale(expgdd_bbd$sm, center=TRUE, scale=TRUE)
expgdd_bbd$smjm_cent<-scale(expgdd_bbd$soilmois_janmar, center = TRUE, scale = TRUE)
expgdd_bbd$ag_min_jm_cent<-scale(expgdd_bbd$ag_min_janmar, center = TRUE, scale = TRUE)
expgdd_bbd$agtmax_cent<-scale(expgdd_bbd$agtmax, center = TRUE, scale = TRUE)


#2) Make a list out of the processed data. It will be input for the model.

datalist.bbd <- with(expgdd_bbd, 
                    list(y = doy, 
                         temp = ag_min_jm_cent[,1], #above-ground minimum air temp
                         mois = smjm_cent[,1], #soil moisture
                         sp = genus.species,
                         N = nrow(expgdd_bbd),
                         n_sp = length(unique(expgdd_bbd$genus.species))
                    )
)

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
                           mois = smjm_cent[,1], #soil moisture as a percentage
                           n_sp = length(unique(expgdd_bbd$genus.species)),
                           n_site = length(unique(expgdd_bbd$site)),
                           N = nrow(expgdd_bbd)
                           )
                      )

m4 = stan('Analyses/soilmoisture/M4_bbd_testdata.stan', data = datalist.bbd,
          iter = 2500, warmup=1500) # 
