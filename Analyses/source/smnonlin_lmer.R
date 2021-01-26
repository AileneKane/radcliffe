# Fitting some nonlinear models in lmer

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

# testm5cent.intsq.rstan2<-stan_lmer(
#     y~ mois*(temp + I(temp^2)) + #fixed
#       (mois*(temp + I(temp^2))|sp)+ (1|site/year),#random
#     data=bbd.cent)#data, etc




# fe <- summary(testm5cent.intsq.lmer2)$coefficients[1:6,1]
# #below doesnt' work
# quartz()
# plot_df <- expand.grid(temp=datalist.bbd.cent$temp, mois=c(min(datalist.bbd.cent$mois), mean(datalist.bbd.cent$mois), max(datalist.bbd.cent$mois))) %>%
#   mutate(doy = fe[1] + fe[2]*mois + fe[3]*temp + fe[4]*temp*temp + fe[5]*mois*temp + fe[6]*mois*temp*temp)
# plot_interaction %+% plot_df
#
# #look at quadratic fit:
# quartz()
# ggplot(expgdd_bbd, aes(ag_min_jm_cent, doy)) + geom_point() + geom_smooth(method="lm", formula=y~poly(x, 2))
#
# summary(testm5cent.lmer)
#
# testm5cent.lmer<-lmer(y~temp * mois +
#                         (temp*mois|sp)+ (1|site/year),
#                       data=datalist.bbd.cent)
# summary(testm5cent.lmer)
#
#
#now try fitting quadratic model in rstanarm


#model fits- onyl when centered! a=96.4445,temp= -9.3089; mois=-1.5954; temp:mois= 0.7739
#try adding year as a fixed effect
# testm5yr.lmer<-lmer(y~temp * mois * year +
#                        (temp*mois |sp)+ (1|site/year),
#                      data=datalist.bbd.cent)#has higher aic than model without year
# summary(testm5yr.lmer)
# #testm5yr2.lmer<-lmer(y~temp * mois * year +
# #                    (temp*mois*year |sp)+ (1|site/year),
# #                  data=datalist.bbd)#doesn't converge
# AIC(testm5.lmer,testm5yr.lmer)
# #look at year as a predictor

#testm5cont.lmer<-lmer(y~temp * mois +
#                    (temp*mois|sp)+ (1|site/year),
#                  data=datalist.bbdcont)
#summary(testm5cont.lmer)
# testm5contcent.lmer<-lmer(y~temp * mois +
#                         (temp*mois|sp)+ (1|site/year),
#                       data=datalist.bbdcont.cent)
# summary(testm5contcent.lmer)
#
# #compare coefs from full dataset and control dataset
# comp.coefs.lmer<-cbind(round(fixef(testm5cent.lmer), digits=3),round(fixef(testm5contcent.lmer), digits=3))
# colnames(comp.coefs.lmer)<-c("m5.cent","m5cont.cent")
# write.csv(comp.coefs.lmer,"Analyses/soilmoisture/comp.coefs.lmer.csv", row.names = TRUE)
# Anova(testm5contcent.lmer)
# Anova(testm5cent.lmer)

#compare coefs in with soil temp versus air temp
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
#
# #look at bbdoy
#
# #Now look ata temperature
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
