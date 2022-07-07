#This file makes a table of model summaries with 90 % credible intervals for the soil moisture ms supplement
load("Analyses/output/brms/testm5cent.brms.bb.Rda")
load("Analyses/output/brms/testm5cent.brms.lo.Rda")
load("Analyses/output/brms/testm5cent.brms.ff.Rda")

bbmod<-testm5cent.brms 
bbsum<-summary(bbmod,prob =.9)
lomod<-testm5cent.lod.brms 
losum<-summary(lomod,prob =.9)
flmod<-testm5cent.ffd.brms 
flsum<-summary(flmod,prob =.9)

bbfix<-as.data.frame(round(cbind(bbsum$fixed[,1],bbsum$fixed[,3],bbsum$fixed[,4]), digits=3))
lofix<-as.data.frame(round(cbind(losum$fixed[,1],losum$fixed[,3],losum$fixed[,4]), digits=3))
flfix<-as.data.frame(round(cbind(flsum$fixed[,1],flsum$fixed[,3],flsum$fixed[,4]), digits=3))

bbran<-as.data.frame(round(rbind(
  cbind(bbsum$random$site[,1],bbsum$random$site[,3],bbsum$random$site[,4]),
  cbind(bbsum$random$`site:year`[,1],bbsum$random$`site:year`[,3],bbsum$random$`site:year`[,4]),
  cbind(bbsum$random$sp[1:4,1],bbsum$random$sp[1:4,3],bbsum$random$sp[1:4,4])),digits=3))

loran<-as.data.frame(round(rbind(
  cbind(losum$random$site[,1],losum$random$site[,3],losum$random$site[,4]),
  cbind(losum$random$`site:year`[,1],losum$random$`site:year`[,3],losum$random$`site:year`[,4]),
  cbind(losum$random$sp[1:4,1],losum$random$sp[1:4,3],losum$random$sp[1:4,4])),digits=3))

flran<-as.data.frame(round(rbind(
  cbind(flsum$random$site[,1],flsum$random$site[,3],flsum$random$site[,4]),
  cbind(flsum$random$`site:year`[,1],flsum$random$`site:year`[,3],flsum$random$`site:year`[,4]),
  cbind(flsum$random$sp[1:4,1],flsum$random$sp[1:4,3],flsum$random$sp[1:4,4])),digits=3))

rownames(bbran)[1:2]<-rownames(loran)[1:2]<-rownames(flran)[1:2]<-c("Site.sd(Intercept)","SiteYr.sd(Intercept)")

bbn<- cbind(c(bbsum$ngrps$site,bbsum$ngrps$`site:year`,bbsum$ngrps$sp,bbsum$nobs),c("","","",""),c("","","",""))
lon<- cbind(c(losum$ngrps$site,losum$ngrps$`site:year`,losum$ngrps$sp,bbsum$nobs),c("","","",""),c("","","",""))
fln<- cbind(c(flsum$ngrps$site,flsum$ngrps$`site:year`,flsum$ngrps$sp,bbsum$nobs),c("","","",""),c("","","",""))

fix<-cbind(bbfix,lofix,flfix)
ran<-cbind(bbran,loran,flran)
n<-cbind(bbn,lon,fln)
rownames(n)<-c("Nsite","Nsite:yr","Nsp","Nobs")
colnames(fix)<-colnames(ran)<-colnames(n)<-rep(c("mean","10%", "90%"),times=3)
tab<-rbind(fix,ran,n)

##row.names(fitallsp.z)[1]<-c("$\\mu_\\alpha$")

nonzmodtable<-cbind(nonztab,nonzcptab,nonzallsptab)
row.names(zmodtable)<-row.names(nonzmodtable)<-c("$\\mu_{\\alpha}$","$\\mu_{forcing}$","$\\mu_{photoperiod}$",   
                           "$\\mu_{chilling}$","$\\sigma_{\\alpha}$", "$\\sigma_{forcing}$"
                           , "$\\sigma_{photoperiod}$","$\\sigma_{chilling}$","$\\sigma_{y}$","$N_{sp}$") 
write.csv(zmodtable,"../../analyses/output/supptables/zmodetable.csv", row.names = FALSE)
write.csv(nonzmodtable,"../../analyses/output/supptables/nonzmodetable.csv", row.names = FALSE)
#zmodtable<-rbind(c("","Utah.units","","","","","Chill.portions","","","","","All.species","","","",""),zmodtable)

#nonzmodtable<-rbind(c("","Utah.units","","","","","Chill.portions","","","","","All.species","","","",""),nonzmodtable)

#load model fit to data minus zohner

#utah units, z
load("../../analyses/bb_analysis/stan/output/m2lni_spcompexprampfputah_nozohner_z.Rda") # m2l.ni
fit.nozohz <- summary(m2l.ni)$summary
#summary(fit.z)# min n_ef: 1198 

nozohztab<-as.data.frame(round(cbind(fit.nozohz[1:9,1],fit.nozohz[1:9,5],fit.nozohz[1:9,7],fit.nozohz[1:9,4],fit.nozohz[1:9,8]),digits=2))
nozohztab<-rbind(nozohztab,c(length(fit.nozohz[grep("a_sp", rownames(fit.nozohz)),1])-2,"","","","",""))

row.names(nozohztab)<-c("$\\mu_{\\alpha}$","$\\mu_{forcing}$","$\\mu_{photoperiod}$",   
                                                 "$\\mu_{chilling}$","$\\sigma_{\\alpha}$", "$\\sigma_{forcing}$"
                                                 , "$\\sigma_{photoperiod}$","$\\sigma_{chilling}$","$\\sigma_{y}$","$N_{sp}$")
colnames(nozohztab)<- c("mean","25%", "75%","2.5%","97.5%")
write.csv(nozohztab,"../../analyses/output/supptables/nozohnzmodtable.csv", row.names = FALSE)

#load model fit with only studies that include atleast 2 interactions
load("../../analyses/bb_analysis/stan/output/m2lni_spcompexprampfputah_z_sm.Rda") # m2l.ni
fit.sm <- summary(m2l.ni)$summary
#summary(fit.z)# min n_ef: 1198 

smtab<-as.data.frame(round(cbind(fit.sm[1:9,1],fit.sm[1:9,5],fit.sm[1:9,7],fit.sm[1:9,4],fit.sm[1:9,8]),digits=2))

smtab<-rbind(smtab,c(length(fit.sm[grep("a_sp", rownames(fit.sm)),1])-2,"","","","",""))

row.names(smtab)<-c("$\\mu_{\\alpha}$","$\\mu_{forcing}$","$\\mu_{photoperiod}$",   
                        "$\\mu_{chilling}$","$\\sigma_{\\alpha}$", "$\\sigma_{forcing}$"
                        , "$\\sigma_{photoperiod}$","$\\sigma_{chilling}$","$\\sigma_{y}$","$N_{sp}$")
colnames(smtab)<- c("mean","25%", "75%","2.5%","97.5%")
write.csv(smtab,"../../analyses/output/supptables/smmodtable.csv", row.names = FALSE)
