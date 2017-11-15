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
sigma_a<-20
mu_b_temp_sp<--2
sigma_b_temp_sp<-.2
mu_b_mois_sp<--1
sigma_b_mois_sp<-.1


a_sp<-as.integer(rnorm(n_sp,mu_a,sigma_a))#species specific day of year for bb

b_temp<-rnorm(n_sp,mu_b_temp_sp,sigma_b_temp_sp)#species specific effects of temp
b_mois<-rnorm(n_sp,mu_b_mois_sp,sigma_b_mois_sp)#species specific effects of mois

#create explanatory variables
temp<-rep(NA, N)
for(i in 1:n_sp){
  temp[which(sp==i)]<-rnorm(obs_sp,25,5)#right now this is set up for each species but it doesn't need to be right?
}
mois<-rep(NA, N)
for(i in 1:n_sp){
  mois[which(sp==i)]<-rnorm(obs_sp,20,1)
}

#temp<-rep(rnorm(obs_sp,25,5), n_sp)
#mois<-rep(rnorm(obs_sp,.2,.1),n_sp)

#b_temp<--2#effect of temp
#b_mois<-2#effect of soil moisture
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
testm1.lmer<-lmer(y~temp + mois +(temp + mois|sp))
summary(testm1.lmer)#looks good!

#now fit the model in stan
testm1 = stan('Analyses/soilmoisture/M1_bbd_testdata.stan', data=list(y=y,sp=sp,temp=temp, mois=mois, n_sp=n_sp,N=N),
              iter = 2500, warmup=1500) # 
beta_draws<-as.matrix(testm1,pars=c("b_temp","b_mois","sigma_y"))
mcmc_intervals(beta_draws)
head(summary(testm1)$summary)
summary(testm1)$summary
launch_shinystan(testm1)#this can be slow

#now try model with interaction:
mu_b_tm_sp<-.005
sigma_b_tm_sp<-.0001
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
testm2.lmer<-lmer(y~temp * mois +(temp*mois|sp))
summary(testm2.lmer)#model fails to converge

#now fit the model in stan
testm2 = stan('Analyses/soilmoisture/M2_bbd_testdata.stan', data=list(y=y,sp=sp,temp=temp, mois=mois,n_sp=n_sp,N=N),
              iter = 2500, warmup=1500) # 



beta_draws<-as.matrix(testm2,pars=c("b_temp","b_mois","mu_b_tm_sp","sigma_y"))
mcmc_intervals(beta_draws)
head(summary(testm2)$summary)
launch_shinystan(testm2)#this can be slow


###Now with the data
source('Analyses/soilmoisture/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

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
#expgdd_bbd$sm_cent <- scale(expgdd_bbd$sm, center=TRUE, scale=TRUE)
#expgdd_bbd$smjm_cent<-scale(expgdd_bbd$soilmois_janmar, center = TRUE, scale = TRUE)
#expgdd_bbd$ag_min_jm_cent<-scale(expgdd_bbd$ag_min_janmar, center = TRUE, scale = TRUE)
#expgdd_bbd$agtmax_cent<-scale(expgdd_bbd$agtmax, center = TRUE, scale = TRUE)


#2) Make a list out of the processed data. It will be input for the model.

datalist.bbd <- with(expgdd_bbd, 
                    list(y = doy, 
                         temp = ag_min_janmar, #above-ground minimum air temp
                         mois = soilmois_janmar*100, #soil moisture as a percentage
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

