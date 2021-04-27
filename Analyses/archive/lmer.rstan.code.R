#SCratch/uused code from model fitting

# testm5.lmer<-lmer(y~temp * mois +
#                     (temp*mois|sp)+ (1|site/year),
#                   REML=FALSE,
#                   data=datalist.bbd)
# summary(testm5.lmer)
#fails to converge wihtout conifers

# Anova(testm5.lmer)
# #model fits! a=108.961,temp= -3.734; mois=--33.070; temp:mois= 2.954
# #try adding year as a fixed effect
# testm5yr.lmer<-lmer(y~temp * mois * year +
#                       (temp*mois |sp)+ (1|site/year),
#                     data=datalist.bbd)#lower aic, interactions between year and temp and year and mois are significant
# 
# testm5yr2.lmer<-lmer(y~temp * mois * year +
#                        (temp*mois* year |sp)+ (1|site/year),
#                     data=datalist.bbd)#doesn't converge
# AIC(testm5.lmer,testm5yr.lmer)
# 
#  testm5cent.lmer<-lmer(y~temp * mois +
#                          (temp*mois|sp)+ (1|site/year),
#                        REML=FALSE,
#                       data=datalist.bbd.cent)
# summary(testm5cent.lmer)
# Anova(testm5cent.lmer)
# # #model fits! a=99.4208, temp= -10.1869; mois=--1.2633; temp:mois=-0.3990
# testm5cont.lmer<-lmer(y~temp * mois +
#                         (temp*mois|sp)+ (1|site/year),
#                       data=datalist.bbdcont)
# summary(testm5cont.lmer)
# testm5contcent.lmer<-lmer(y~temp * mois +
#                             (temp*mois|sp)+ (1|site/year),
#                           data=datalist.bbdcont.cent)
# summary(testm5contcent.lmer)
# 
# #compare coefs from full dataset and control dataset
# comp.coefs.lmer<-cbind(round(fixef(testm5.lmer), digits=3),round(fixef(testm5cont.lmer), digits=3),round(fixef(testm5cent.lmer), digits=3),round(fixef(testm5contcent.lmer), digits=3))
# colnames(comp.coefs.lmer)<-c("m5","m5cont","m5.cent","m5cont.cent")
# write.csv(comp.coefs.lmer,"Analyses/soilmoisture/comp.coefs.lmer.csv", row.names = TRUE)
# Anova(testm5contcent.lmer)
# Anova(testm5cent.lmer)
# Anova(testm5cont.lmer)
# Anova(testm5.lmer)
# 
# #compare coefs in with soil temp versus air temp
# colnames(expgdd)
# #mois not significant in controls; is marginally significant in full dataset
# #Make plot of range of soil moisture in controls and in all
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# hist(datalist.bbd$mois, main="All plots")
# mn.mois<-round(mean(datalist.bbd$mois), digits=4)
# md.mois<-round(median(datalist.bbd$mois), digits=4)
# 
# var(datalist.bbd$mois)
# mtext(paste(c(mn.mois)))
# 
# hist(datalist.bbdcont$mois, main = "Control plots")
# mn.mois.cont<-round(mean(datalist.bbdcont$mois), digits=4)
# md.mois.cont<-round(median(datalist.bbdcont$mois), digits=4)
# 
# mtext(paste(mn.mois.cont))
# 
# cor(datalist.bbd$temp,datalist.bbd$mois)
# 
# #Now look ata temperature
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# hist(datalist.bbd$temp, main="All plots")
# mn.temp<-round(mean(datalist.bbd$temp), digits=4)
# md.temp<-round(median(datalist.bbd$temp), digits=4)
# 
# var(datalist.bbd$temp)
# mtext(paste(c(mn.temp)))
# 
# hist(datalist.bbdcont$temp, main = "Control plots")
# mn.temp.cont<-round(mean(datalist.bbdcont$temp), digits=4)
# md.temp.cont<-round(median(datalist.bbdcont$temp), digits=4)
# 
# mtext(paste(mn.temp.cont))

#look at bbdoy

#Now look at temperature
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# hist(datalist.bbd$y, main="All plots")
# mn.doy<-round(mean(datalist.bbd$y), digits=4)
# md.doy<-round(median(datalist.bbd$y), digits=4)
# 
# var(datalist.bbd$y)
# mtext(paste(c(mn.doy)))
# 
# hist(datalist.bbdcont$y, main = "Control plots")
# mn.doy.cont<-round(mean(datalist.bbdcont$y), digits=4)
# md.doy.cont<-round(median(datalist.bbdcont$y), digits=4)
# 
# mtext(paste(mn.doy.cont))
# 
# #Now plot doy~ temp for controls and all
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# plot(datalist.bbd$temp,datalist.bbd$y, main="All plots",xlab="temp",ylab="BB doy")
# abline(a=fixef(testm5.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
# plot(datalist.bbdcont$temp,datalist.bbdcont$y, main="Control plots",xlab="temp",ylab="BB doy")
# abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
# 
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# plot(datalist.bbd$mois,datalist.bbd$y, main="All plots",xlab="vwc",ylab="BB doy")
# abline(a=fixef(testm5.lmer)[1],b=fixef(testm5.lmer)[3], col="red")
# plot(datalist.bbdcont$mois,datalist.bbdcont$y, main="Control plots",xlab="vwc",ylab="BB doy")
# abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5.lmer)[3], col="red")
# 
# #Now plot doy~ temp for controls and all
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# plot(datalist.bbd$temp,datalist.bbd$y, main="All plots",xlab="temp",ylab="BB doy")
# abline(a=fixef(testm5.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
# plot(datalist.bbdcont$temp,datalist.bbdcont$y, main="Control plots",xlab="temp",ylab="BB doy")
# abline(a=fixef(testm5cont.lmer)[1],b=fixef(testm5.lmer)[2], col="red")
# #look at correlation between mois and temp
# quartz(height=5,width=7)
# par(mfrow=c(1,2))
# plot(datalist.bbd$temp,datalist.bbd$mois, main="All plots, r=0.32",xlab="temp",ylab="vwc")
# abline(lm(datalist.bbd$mois~datalist.bbd$temp), col="red")
# cor(datalist.bbd$mois,datalist.bbd$temp)#0.32
# plot(datalist.bbdcont$temp,datalist.bbdcont$mois, main="Control plots, r=0.37",xlab="temp",ylab="vwc")
# abline(lm(datalist.bbdcont$mois~datalist.bbdcont$temp), col="red")
# cor(datalist.bbdcont$mois,datalist.bbdcont$temp)#0.37
# 


# stancode(testm5.brms)
# summary(testm5.brms)
# quartz()
# stanplot(testm5.brms, pars = "^b_")
# 
# stanplot(testm5.brms, surface = TRUE)
#22 divergent transitions

# testm5.rstan <- stan_lmer(formula = doy ~ ag_min_janmar * soilmois_janmar +#fixed effects
#                                 ((ag_min_janmar * soilmois_janmar)|genus.species) + (1|site/year), 
#                               data = expgdd_bbd, chains=2)
# save(testm5.rstan, file="Analyses/output/brms/testm5.rstanarm.bb.Rda")


#stanarm
# testm5cent.rstan <- stan_lmer(formula = doy ~ ag_min_jm_cent * smjm_cent +#fixed effects
#                         ((ag_min_jm_cent * smjm_cent)|genus.species) + (1|site/year), 
#                          data = expgdd_bbd, chains=2)
# summary(testm5cent.rstan)
# coef(testm5cent.rstan)
# head(testm5cent.rstan$stan_summary)
# save(testm5cent.rstan, file="Analyses/output/brms/testm5cent.rstanarm.bb.Rda")
