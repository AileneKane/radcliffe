#Started Sept 2017
#By Ailene
#Use brms/Stan to fit soil moisture- phenology model to radcliffe data 
#Three questions to address:
#1) How do warming and precip treatments affect soil moisture? (Make plots and fit models)
#2) How does soil moisture affect GDDcrit?
#3) How do soil moisture and temperature affect doy of bud burst, leaf out, etc?

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
n_sites=10#number of sites
n_yr=2#number of years per site
obs_yr=365#number of obs per year
N=n_sites*n_yr*n_sites#sample size

site<-rep(seq(1:n_site), each=obs_site)#site ids
yr<-rep(seq(1:n_yr), each=obs_yr)#year ids

#set up distribution parameters
mu_a<-180#grand mean of soil moisture measurements
sigma_a_site<-5
mu_b_targ_site<--.1
sigma_b_targ_site<-.001
mu_b_precip_site<-.1
sigma_b_precip_site<-.001
a_site<-as.integer(rnorm(n_site,mu_a,sigma_a_site))#species specific day of year for bb

#M4: With site added as intercept only random effect

b_targ<-rnorm(n_site,mu_b_targ_site,sigma_b_targ_site)#species specific effects of temp
b_prec<-rnorm(n_site,mu_b_precip_site,sigma_b_precip_site)#species specific effects of mois

#create explanatory variables
#try using centered predictors
targtemp<-rep(NA, N)
for(i in 1:n_site){
  for(j in 1:n_yr)
  targtemp[which(site==i)]<-rnorm(obs_site,0,1)#
}
precip<-rep(NA, N)
for(i in 1:n_sp){
  precip[which(site==i)]<-rnorm(obs_site,0,1)#
}

#now fit the model in stan for comparison- haven't done this in a while...
mu_b_tp_site<-.01
sigma_b_tp_site<-.005
b_tp<-rnorm(n_sp,mu_b_tp_site,sigma_b_tm_site)#site specific interaction

ypred<-c()
for(i in 1:N){
  ypred[i] = a_site[site[i]] + b_targ[site[i]] * targ[i] + b_prec[site[i]] * prec[i]+ b_tp[site[i]]*targ[i] * prec[i]
}

y<-rnorm(N,ypred,sigma_y)
#check that test data look ok
plot(targ,y)
plot(prec,y)
hist(prec)


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
                     (temp * mois|site) + (1|year/doy), #random effects
                   data=list(y=y,sp=sp,temp=temp, mois=mois, n_sp=n_sp,N=N, site=site),
                   chains = 2,control = list(max_treedepth = 15,adapt_delta = 0.99)) 
 #the above code is really, really slow....
stancode(testm4.brms)
summary(testm4.brms)
#brms model says a: 299.44, temp=-1.97, mois=-1.03, tmint=-0.11, species sigma=0.51
marginal_effects(testm4.brms, surface = TRUE)
#looks good!!! but took a really long time to fit...


###Now with the data
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

############
#fit models#
############
###Fit lmer model for soil moisture~warming*preciptreatment
sm_mod_cent<-lmer(y~target*prec + 
                  (target*prec|site)+(1|year/doy),
                  REML=TRUE, data=datalist.sm.cent)
summary(sm_mod_cent)
coef(sm_mod_cent)
sm_mod<-lmer(y~target*prec + 
                    (target*prec|site)+(1|year/doy),
                  REML=TRUE, data=datalist.sm)
summary(sm_mod)
coef(sm_mod)
#a=0.205, targ=-0.0089, precip=0.014,targ*precip=0.000366
sm_mod_cent2<-lmer(y~target*prec + 
                    (1|site/year/doy),
                  REML=TRUE, data=datalist.sm.cent)
summary(sm_mod_cent2)
sm_mod2<-lmer(y~target*prec + 
                     (1|site/year/doy),
                   REML=TRUE, data=datalist.sm)
summary(sm_mod2)

#a=0.181, targ=-0.0065, precip=0.0002,targ*precip=0.000029

AIC(sm_mod_cent,sm_mod_cent2)
AIC(sm_mod,sm_mod2)

#sm_mod_cent2 has lower AIC...
#try the model with brms
sm_mod.brms <- brm(y ~ target * prec +#fixed effects
                     (1|site/year/doy), #random effects
                   data=datalist.sm.cent,
                   chains = 2)# control = list(max_treedepth = 15,adapt_delta = 0.99)


stancode(sm_mod.brms)
summary(sm_mod.brms)
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
