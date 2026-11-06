## Started April 2021 ##
## By Ailene ##

## plot species estimates by lifeform and ecosystem

############################################
# housekeeping
rm(list=ls()) 

#libraries
library(RColorBrewer)
library(dplyr)
library(brms)
library(scales)
# Setting working directory.
#setwd("~/Documents/GitHub/radcliffe")
setwd("C:/Users/ailene.ettinger/Documents/GitHub/radcliffe")

figpath <- "Analyses/soilmoisture/figures"


#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=TRUE)
forms<-read.csv("Analyses/output/lifeform.csv", header=TRUE)

#Make some choices about how to restrict the data and which model to use:
remove.conifers=TRUE
use.airtemp=TRUE
use.centmod=TRUE

#standard data wrangling to get expclim2 for climate analyses and expgdd
#for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")

#summarize climate data by plot (annual and seasonal temp, soil mois), 
#merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
source("Analyses/source/climsum_byplot.R")

#Prepare data for phenology model fitting
source("Analyses/source/stanprep_phenmods.R")

##Now something with functional group
source("Analyses/source/get_lifeform.R")

##get ecosystem
source("Analyses/source/get_ecosystem.R")

##functions to make the plots

#plot.lf<-function(mod,lifeform,)

  
#plot posterior density curves by life form

figname<-"curvebbloflform.pdf"
pdf(file.path(figpath,figname), height=12,width=18)
#quartz(height=12,width=18)
#windows(height=12,width=18)
par(mfrow=c(3,3), mar=c(5,10,4,2)+0.1)

#Budburst

bbmod<- load("Analyses/output/brms/testm5cent.brms.bb.Rda")
mod<- testm5cent.brms
fit <- fixef(mod)

#getest.bb.sp <- function(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm, wet2sm,spnum,sitenum){
listofdraws <-as.data.frame(as.matrix(mod))
matchExpression <- paste("r_sp[",bbform$spnumbb[bbform$form=="tree"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
treem<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#check: 
#colnames(treem)
#mois
matchExpression <- paste("r_sp[",bbform$spnumbb[bbform$form=="shrub"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubm<-as.matrix(listofdraws%>% select(all_of(cols2use)))
#temp
matchExpression <- paste("r_sp[",bbform$spnumbb[bbform$form=="tree"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
treet<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",bbform$spnumbb[bbform$form=="shrub"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#temp*mois interaction
matchExpression <- paste("r_sp[",bbform$spnumbb[bbform$form=="tree"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
treeint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",bbform$spnumbb[bbform$form=="shrub"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#quartz(height=10,width=4)
#windows()
dta_tree <- density(treet, na.rm = TRUE)
dta_shrub <- density(shrubt, na.rm = TRUE)

plot(dta_tree, col ="darkred", main = "Temperature", 
     xlim=c(-40,40),
     ylim = c(0, max(dta_tree$y,dta_shrub$y)),
     lwd=2,bty="l", xlab="Effect on budburst per SD",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
lines(dta_shrub, col = "darkblue",lwd=2)
#abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
mtext("A)",side=3,line=1, cex=1.2, adj=0)

legend("topright", c("Trees","Shrubs","Forbs","Grass"), 
       lty = c(1,1), col = c("darkred","darkblue","darkgreen",'goldenrod'),
       lwd=2,bty="n", cex=1.5)
mtext("Budburst",side=2,line=6, cex=1.2)

dta_treem <- density(treem, na.rm = TRUE)
dta_shrubm <- density(shrubm, na.rm = TRUE)

plot(dta_treem, col ="darkred", main = "Moisture", 
     xlim=c(-20,20),
     ylim = c(0, max(dta_treem$y,dta_shrubm$y)),
     lwd=2,bty="l", xlab="Effect on budburst per SD",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
lines(dta_shrubm, col = "darkblue",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
mtext("B)",side=3,line=1, cex=1.2, adj=0)

dta_treeint <- density(treeint, na.rm = TRUE)
dta_shrubint <- density(shrubint, na.rm = TRUE)

plot(dta_treeint, col ="darkred", main = "Temp*Moist", 
     xlim=c(-20,20),
     ylim = c(0, max(dta_treeint$y,dta_shrubint$y)),
     lwd=2,bty="l", xlab="Effect on budburst per SD",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
lines(dta_shrubint, col = "darkblue",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)

#dev.off()
mtext("C)",side=3,line=1, cex=1.2, adj=0)
#save mean, median, quantiles for table

bb_lifeform_posts<-rbind(
    c("budburst","tree","$\\mu_{temp}$",round(mean(treet), digits=2),round(median(treet), digits=2),round(quantile(treet,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("budburst","tree","$\\mu_{mois}$",round(mean(treem), digits=2),round(median(treem), digits=2),round(quantile(treem,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("budburst","tree","$\\mu_{temp:mois}$",round(mean(treeint), digits=2),round(median(treeint), digits=2),round(quantile(treeint,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("budburst","shrub","$\\mu_{temp}$",round(mean(shrubt), digits=2),round(median(shrubt), digits=2),round(quantile(shrubt,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("budburst","shrub","$\\mu_{mois}$",round(mean(shrubm), digits=2),round(median(shrubm), digits=2),round(quantile(shrubm,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("budburst","shrub","$\\mu_{temp:mois}$",round(mean(shrubint), digits=2),round(median(shrubint), digits=2),round(quantile(shrubint,probs=c(0.1,.25,.75,.9)), digits=2))
) 
    
#Leaf-Out
lomod<- load("Analyses/output/brms/testm5cent.brms.lo.Rda")
mod<- testm5cent.lod.brms

fit <- fixef(mod)
#getest.bb.sp <- function(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm, wet2sm,spnum,sitenum){
listofdraws <-as.data.frame(as.matrix(mod))
matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="tree"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
treem<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#check: 
#colnames(treem)
#mois
matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="shrub"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="forb"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forbm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="grass"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grassm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#temp
matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="tree"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
treet<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="shrub"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubt<-as.matrix(listofdraws%>% select(all_of(cols2use)))
#mean(shrubt)
#quantile(shrubt,probs=c(0.1,.9))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="forb"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forbt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="grass"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grasst<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#temp*mois interaction
matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="tree"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
treeint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="shrub"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="forb"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forbint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loform$spnumlo[loform$form=="grass"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grassint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

dta_tree <- density(treet, na.rm = TRUE)
dta_shrub <- density(shrubt, na.rm = TRUE)
dta_forb <- density(forbt, na.rm = TRUE)
dta_grass <- density(grasst, na.rm = TRUE)

plot(dta_forb, col ="darkgreen", main = " ", 
     xlim=c(-40,40),
    ylim = c(0, max(dta_forb$y,dta_tree$y,dta_shrub$y,dta_grass$y)),
    lwd=2,bty="l", xlab="Effect on leafout per SD",
    cex.axis=1.5, cex.lab=1.5)
lines(dta_tree, col = "darkred",lwd=2)
lines(dta_shrub, col = "darkblue",lwd=2)
lines(dta_grass, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
mtext("Leafout",side=2,line=6, cex=1.2)
mtext("D)",side=3,line=1, cex=1.2, adj=0)

#legend("topright", c("Trees","Shrubs","Forbs","Grass"), lty = c(1,1), col = c("darkgreen","darkblue","darkred",'goldenrod'), lwd=2,bty="n")

dta_treem <- density(treem, na.rm = TRUE)
dta_shrubm <- density(shrubm, na.rm = TRUE)
dta_forbm <- density(forbm, na.rm = TRUE)
dta_grassm <- density(grassm, na.rm = TRUE)

plot(dta_forbm, col ="darkgreen", main = " ", 
     xlim=c(-20,20),
     ylim = c(0, max(dta_forbm$y,dta_treem$y,dta_shrubm$y,dta_grassm$y)),
     lwd=2,bty="l", xlab="Effect on leafout per SD",
     cex.axis=1.5, cex.lab=1.5)
lines(dta_treem, col = "darkred",lwd=2)
lines(dta_shrubm, col = "darkblue",lwd=2)
lines(dta_grassm, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
mtext("E)",side=3,line=1, cex=1.2, adj=0)

dta_treeint <- density(treeint, na.rm = TRUE)
dta_shrubint <- density(shrubint, na.rm = TRUE)
dta_forbint <- density(forbint, na.rm = TRUE)
dta_grassint<- density(grassint, na.rm = TRUE)

plot(dta_forbint, col ="darkgreen", main = " ", 
     xlim=c(-20,20),
     ylim = c(0, max(dta_forbint$y,dta_treeint$y,dta_shrubint$y,dta_grassint$y)),
     lwd=2,bty="l", xlab="Effect on leafout per SD",
     cex.axis=1.5, cex.lab=1.5)
lines(dta_treeint, col = "darkred",lwd=2)
lines(dta_shrubint, col = "darkblue",lwd=2)
lines(dta_grassint, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
mtext("F)",side=3,line=1, cex=1.2, adj=0)

#save posteriors in table
lo_lifeform_posts<-rbind(
  c("leafout","tree","$\\mu_{temp}$",round(mean(treet), digits=2),round(median(treet), digits=2),round(quantile(treet,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("leafout","tree","$\\mu_{mois}$",round(mean(treem), digits=2),round(median(treem), digits=2),round(quantile(treem,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("leafout","tree","$\\mu_{temp:mois}$",round(mean(treeint), digits=2),round(median(treeint), digits=2),round(quantile(treeint,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("leafout","shrub","$\\mu_{temp}$",round(mean(shrubt), digits=2),round(median(shrubt), digits=2),round(quantile(shrubt,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("leafout","shrub","$\\mu_{mois}$",round(mean(shrubm), digits=2),round(median(shrubm), digits=2),round(quantile(shrubm,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("leafout","shrub","$\\mu_{temp:mois}$",round(mean(shrubint), digits=2),round(median(shrubint), digits=2),round(quantile(shrubint,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("leafout","forb","$\\mu_{temp}$",round(mean(forbt), digits=2),round(median(forbt), digits=2),round(quantile(forbt,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("leafout","forb","$\\mu_{mois}$",round(mean(forbm), digits=2),round(median(forbm), digits=2),round(quantile(forbm,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("leafout","forb","$\\mu_{temp:mois}$",round(mean(forbint), digits=2),round(median(forbint), digits=2),round(quantile(forbint,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("leafout","grass","$\\mu_{temp}$",round(mean(grasst), digits=2),round(median(grasst), digits=2),round(quantile(grasst,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("leafout","grass","$\\mu_{mois}$",round(mean(grassm), digits=2),round(median(grassm), digits=2),round(quantile(grassm,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("leafout","grass","$\\mu_{temp:mois}$",round(mean(grassint), digits=2),round(median(grassint), digits=2),round(quantile(grassint,probs=c(0.1,.25,.75,.9)), digits=2))
)



#dev.off()

#Flowering

flmod<- load("Analyses/output/brms/testm5cent.brms.ff.Rda")
mod<- testm5cent.ffd.brms
fit <- fixef(mod)
listofdraws <-as.data.frame(as.matrix(mod))
#no flowering stages for trees 
#mois
matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="shrub"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubm<-as.matrix(listofdraws%>% select(all_of(cols2use)))
#check: 
#colnames(shrubm)

matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="forb"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forbm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="grass"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grassm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#temp

matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="shrub"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubt<-as.matrix(listofdraws%>% select(all_of(cols2use)))
#mean(shrubt)
#quantile(shrubt,probs=c(0.1,.9))

matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="forb"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forbt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="grass"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grasst<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#temp*mois interaction

matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="shrub"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
shrubint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="forb"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forbint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",flform$spnumfl[flform$form=="grass"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grassint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

dta_shrub <- density(shrubt, na.rm = TRUE)
dta_forb <- density(forbt, na.rm = TRUE)
#dta_grass <- density(grasst, na.rm = TRUE)#not sure why grasses are not in model dataset

plot(dta_forb, col ="darkgreen", main = " ", 
     xlim=c(-40,40),
     ylim = c(0, max(dta_forb$y,dta_tree$y,dta_shrub$y,dta_grass$y)),
     lwd=2,bty="l", xlab="Effect on flowering per SD",
     cex.axis=1.5, cex.lab=1.5)
lines(dta_shrub, col = "darkblue",lwd=2)
#lines(dta_grass, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
mtext("Flowering",side=2,line=6, cex=1.2)
mtext("G)",side=3,line=1, cex=1.2, adj=0)

#legend("topright", c("Trees","Shrubs","Forbs","Grass"), lty = c(1,1), col = c("darkgreen","darkblue","darkred",'goldenrod'), lwd=2,bty="n")

dta_shrubm <- density(shrubm, na.rm = TRUE)
dta_forbm <- density(forbm, na.rm = TRUE)
#dta_grassm <- density(grassm, na.rm = TRUE)

plot(dta_forbm, col ="darkgreen", main = " ", 
     xlim=c(-20,20),
     ylim = c(0, max(dta_forbm$y,dta_treem$y,dta_shrubm$y,dta_grassm$y)),
     lwd=2,bty="l", xlab="Effect on flowering per SD",
     cex.axis=1.5, cex.lab=1.5)
lines(dta_shrubm, col = "darkblue",lwd=2)
#lines(dta_grassm, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
mtext("H)",side=3,line=1, cex=1.2, adj=0)

dta_shrubint <- density(shrubint, na.rm = TRUE)
dta_forbint <- density(forbint, na.rm = TRUE)
#dta_grassint<- density(grassint, na.rm = TRUE)

plot(dta_forbint, col ="darkgreen", main = " ", 
     xlim=c(-20,20),
     ylim = c(0, max(dta_forbint$y,dta_treeint$y,dta_shrubint$y,dta_grassint$y)),
     lwd=2,bty="l", xlab="Effect on flowering per SD",
     cex.axis=1.5, cex.lab=1.5)
lines(dta_shrubint, col = "darkblue",lwd=2)
#lines(dta_grassint, col = "goldenrod",lwd=2)
#abline(v=fixef(mod)[3,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
mtext("I)",side=3,line=1, cex=1.2, adj=0)

dev.off()

#flowering posteriors
fl_lifeform_posts<-rbind(
  c("flowering","shrub","$\\mu_{temp}$",round(mean(shrubt), digits=2),round(median(shrubt), digits=2),round(quantile(shrubt,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("flowering","shrub","$\\mu_{mois}$",round(mean(shrubm), digits=2),round(median(shrubm), digits=2),round(quantile(shrubm,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("flowering","shrub","$\\mu_{temp:mois}$",round(mean(shrubint), digits=2),round(median(shrubint), digits=2),round(quantile(shrubint,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("flowering","forb","$\\mu_{temp}$",round(mean(forbt), digits=2),round(median(forbt), digits=2),round(quantile(forbt,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("flowering","forb","$\\mu_{mois}$",round(mean(forbm), digits=2),round(median(forbm), digits=2),round(quantile(forbm,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("flowering","forb","$\\mu_{temp:mois}$",round(mean(forbint), digits=2),round(median(forbint), digits=2),round(quantile(forbint,probs=c(0.1,.25,.75,.9)), digits=2)) 
)

#table of all phenophases
allphen_lifeform_posts<-rbind(
  bb_lifeform_posts,
  lo_lifeform_posts,
  fl_lifeform_posts
)
colnames(allphen_lifeform_posts)[1:5]<- c("phenophase","lifeform","predictor","mean","50%")
write.csv(allphen_lifeform_posts,"Analyses/soilmoisture/lifeform_table.csv",row.names = FALSE)

#####################################
####### Ecosystem differences #######
#####################################

figname<-"curvesbbloflecos.pdf"

pdf(file.path(figpath,figname), height=8,width=18)
#windows()
par(mfrow=c(3,3))

#budburst
bbmod<- load("Analyses/output/brms/testm5cent.brms.bb.Rda")
mod<- testm5cent.brms
fit <- fixef(mod)

listofdraws <-as.data.frame(as.matrix(mod))

#moisture
matchExpression <- paste("r_sp[",bbecos$spnum[bbecos$ecosystem=="forest"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forestm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",bbecos$spnum[bbecos$ecosystem=="grassland"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grasslandm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#check: 
#colnames(forestm)

#temperature
matchExpression <- paste("r_sp[",bbecos$spnum[bbecos$ecosystem=="forest"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forestt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",bbecos$spnum[bbecos$ecosystem=="grassland"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grasslandt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#temp*mois interaction
matchExpression <- paste("r_sp[",bbecos$spnum[bbecos$ecosystem=="forest"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forestint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",bbecos$spnum[bbecos$ecosystem=="grassland"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grasslandint<-as.matrix(listofdraws%>% select(all_of(cols2use)))


dta_forest <- density(forestt, na.rm = TRUE)
dta_grassland <- density(grasslandt, na.rm = TRUE)

plot(dta_forest, col ="forestgreen", main = "Temperature", 
     xlim=c(-40,40),
     ylim = c(0, max(dta_forest$y,dta_grassland$y)),
     lwd=3,bty="l", xlab="Effect on budburst per SD",
     cex.axis=1.5, cex.lab=1.5)
lines(dta_grassland, col = "goldenrod",lwd=3)
#abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
mtext("Leafout",side=2,line=6, cex=1.2)
mtext("A)",side=3,line=1, cex=1.2, adj=0)

legend("topright", c("Forest","Grassland"), lty = c(1,1), col = c("forestgreen",'goldenrod'), lwd=2,bty="n")

dta_forest <- density(forestt, na.rm = TRUE)
dta_grassland <- density(grasslandt, na.rm = TRUE)

#moisture
dta_forestm <- density(forestm, na.rm = TRUE)
dta_grasslandm <- density(grasslandm, na.rm = TRUE)

plot(dta_forestm, col ="forestgreen", main = "Moisture", 
     xlim=c(-20,20),
     ylim = c(0, max(dta_forestm$y,dta_grasslandm$y)),
     lwd=3,bty="l", xlab="Effect on budburst per SD",
     cex.axis=1.5, cex.lab=1.5)
lines(dta_grasslandm, col = "goldenrod",lwd=3)
#abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
mtext("Budburst",side=2,line=6, cex=1.2)
mtext("B)",side=3,line=1, cex=1.2, adj=0)

#interaction
dta_forestint <- density(forestint, na.rm = TRUE)
dta_grasslandint <- density(grasslandint, na.rm = TRUE)

plot(dta_forestint, col ="forestgreen", main = "Temperature*Moisture", 
     xlim=c(-20,20),
     ylim = c(0, max(dta_forestint$y,dta_grasslandint$y)),
     lwd=3,bty="l", xlab="Effect on budburst per SD",
     cex.axis=1.5, cex.lab=1.5)
lines(dta_grasslandint, col = "goldenrod",lwd=3)
#abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
abline(v=0, lwd=1,col="black",lty=2)
mtext("Budburst",side=2,line=6, cex=1.2)
mtext("C)",side=3,line=1, cex=1.2, adj=0)

#save posteriors in table
bb_ecosystem_posts<-rbind(
  c("budburst","forest","$\\mu_{temp}$",round(mean(forestt), digits=2),round(median(forestt), digits=2),round(quantile(forestt,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("budburst","forest","$\\mu_{mois}$",round(mean(forestm), digits=2),round(median(forestm), digits=2),round(quantile(forestm,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("budburst","forest","$\\mu_{temp:mois}$",round(mean(forestint), digits=2),round(median(forestint), digits=2),round(quantile(forestint,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("budburst","grassland","$\\mu_{temp}$",round(mean(grasslandt), digits=2),round(median(grasslandt), digits=2),round(quantile(grasslandt,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("budburst","grassland","$\\mu_{mois}$",round(mean(grasslandm), digits=2),round(median(grasslandm), digits=2),round(quantile(grasslandm,probs=c(0.1,.25,.75,.9)), digits=2)),
  c("budburst","grassland","$\\mu_{temp:mois}$",round(mean(grasslandint), digits=2),round(median(grasslandint), digits=2),round(quantile(grasslandint,probs=c(0.1,.25,.75,.9)), digits=2))
)

#Leafout
mod<- testm5cent.lod.brms
fit <- fixef(mod)

listofdraws <-as.data.frame(as.matrix(mod))

#moisture
matchExpression <- paste("r_sp[",loecos$spnum[loecos$ecosystem=="forest"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forestm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loecos$spnum[loecos$ecosystem=="grassland"],",mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grasslandm<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#check: 
#colnames(forestm)

#temperature
matchExpression <- paste("r_sp[",loecos$spnum[loecos$ecosystem=="forest"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forestt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loecos$spnum[loecos$ecosystem=="grassland"],",temp]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grasslandt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

#temp*mois interaction
matchExpression <- paste("r_sp[",loecos$spnum[loecos$ecosystem=="forest"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
forestint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

matchExpression <- paste("r_sp[",loecos$spnum[loecos$ecosystem=="grassland"],",temp:mois]", collapse = NULL,sep="")
cols2use<-intersect(colnames(listofdraws),matchExpression)
grasslandint<-as.matrix(listofdraws%>% select(all_of(cols2use)))

  dta_forest <- density(forestt, na.rm = TRUE)
  dta_grassland <- density(grasslandt, na.rm = TRUE)
  
  plot(dta_forest, col ="forestgreen", main = "Temperature", 
       xlim=c(-40,40),
       ylim = c(0, max(dta_forest$y,dta_grassland$y)),
       lwd=3,bty="l", xlab="Effect on leafout per SD",
       cex.axis=1.5, cex.lab=1.5)
  lines(dta_grassland, col = "goldenrod",lwd=3)
  #abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
  abline(v=0, lwd=1,col="black",lty=2)
  mtext("Leafout",side=2,line=6, cex=1.2)
  mtext("D)",side=3,line=1, cex=1.2, adj=0)
  
  
  dta_forest <- density(forestt, na.rm = TRUE)
  dta_grassland <- density(grasslandt, na.rm = TRUE)
  
  #moisture
  dta_forestm <- density(forestm, na.rm = TRUE)
  dta_grasslandm <- density(grasslandm, na.rm = TRUE)
  
  plot(dta_forestm, col ="forestgreen", main = "Moisture", 
       xlim=c(-20,20),
       ylim = c(0, max(dta_forestm$y,dta_grasslandm$y)),
       lwd=3,bty="l", xlab="Effect on leafout per SD",
       cex.axis=1.5, cex.lab=1.5)
  lines(dta_grasslandm, col = "goldenrod",lwd=3)
  #abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
  abline(v=0, lwd=1,col="black",lty=2)
  mtext("Leafout",side=2,line=6, cex=1.2)
  mtext("E)",side=3,line=1, cex=1.2, adj=0)
  
  #interaction
  dta_forestint <- density(forestint, na.rm = TRUE)
  dta_grasslandint <- density(grasslandint, na.rm = TRUE)
  
  plot(dta_forestint, col ="forestgreen", main = "Temperature*Moisture", 
       xlim=c(-20,20),
       ylim = c(0, max(dta_forestint$y,dta_grasslandint$y)),
       lwd=3,bty="l", xlab="Effect on leafout per SD",
       cex.axis=1.5, cex.lab=1.5)
  lines(dta_grasslandint, col = "goldenrod",lwd=3)
  #abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
  abline(v=0, lwd=1,col="black",lty=2)
  mtext("Leafout",side=2,line=6, cex=1.2)
  mtext("F)",side=3,line=1, cex=1.2, adj=0)
  
  #save posteriors in table
  lo_ecosystem_posts<-rbind(
    c("leafout","forest","$\\mu_{temp}$",round(mean(forestt), digits=2),round(median(forestt), digits=2),round(quantile(forestt,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("leafout","forest","$\\mu_{mois}$",round(mean(forestm), digits=2),round(median(forestm), digits=2),round(quantile(forestm,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("leafout","forest","$\\mu_{temp:mois}$",round(mean(forestint), digits=2),round(median(forestint), digits=2),round(quantile(forestint,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("leafout","grassland","$\\mu_{temp}$",round(mean(grasslandt), digits=2),round(median(grasslandt), digits=2),round(quantile(grasslandt,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("leafout","grassland","$\\mu_{mois}$",round(mean(grasslandm), digits=2),round(median(grasslandm), digits=2),round(quantile(grasslandm,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("leafout","grassland","$\\mu_{temp:mois}$",round(mean(grasslandint), digits=2),round(median(grasslandint), digits=2),round(quantile(grasslandint,probs=c(0.1,.25,.75,.9)), digits=2))
  )
  

#Flowering
  
  flmod<- load("Analyses/output/brms/testm5cent.brms.ff.Rda")
  mod<- testm5cent.ffd.brms
  fit <- fixef(mod)
  listofdraws <-as.data.frame(as.matrix(mod))
  
  #moisture
  matchExpression <- paste("r_sp[",ffecos$spnum[ffecos$ecosystem=="forest"],",mois]", collapse = NULL,sep="")
  cols2use<-intersect(colnames(listofdraws),matchExpression)
  forestm<-as.matrix(listofdraws%>% select(all_of(cols2use)))
  
  matchExpression <- paste("r_sp[",ffecos$spnum[ffecos$ecosystem=="grassland"],",mois]", collapse = NULL,sep="")
  cols2use<-intersect(colnames(listofdraws),matchExpression)
  grasslandm<-as.matrix(listofdraws%>% select(all_of(cols2use)))
  
  #check: 
  #colnames(forestm)
  
  #temperature
  matchExpression <- paste("r_sp[",ffecos$spnum[ffecos$ecosystem=="forest"],",temp]", collapse = NULL,sep="")
  cols2use<-intersect(colnames(listofdraws),matchExpression)
  forestt<-as.matrix(listofdraws%>% select(all_of(cols2use)))

  matchExpression <- paste("r_sp[",ffecos$spnum[ffecos$ecosystem=="grassland"],",temp]", collapse = NULL,sep="")
  cols2use<-intersect(colnames(listofdraws),matchExpression)
  grasslandt<-as.matrix(listofdraws%>% select(all_of(cols2use)))
  
  #temp*mois interaction
  matchExpression <- paste("r_sp[",ffecos$spnum[ffecos$ecosystem=="forest"],",temp:mois]", collapse = NULL,sep="")
  cols2use<-intersect(colnames(listofdraws),matchExpression)
  forestint<-as.matrix(listofdraws%>% select(all_of(cols2use)))
  
  matchExpression <- paste("r_sp[",ffecos$spnum[ffecos$ecosystem=="grassland"],",temp:mois]", collapse = NULL,sep="")
  cols2use<-intersect(colnames(listofdraws),matchExpression)
  grasslandint<-as.matrix(listofdraws%>% select(all_of(cols2use)))
  
  dta_forest <- density(forestt, na.rm = TRUE)
  dta_grassland <- density(grasslandt, na.rm = TRUE)
  
  plot(dta_forest, col ="forestgreen", main = "Temperature", 
       xlim=c(-40,40),
       ylim = c(0, max(dta_forest$y,dta_grassland$y)),
       lwd=3,bty="l", xlab="Effect on flowering per SD",
       cex.axis=1.5, cex.lab=1.5)
  lines(dta_grassland, col = "goldenrod",lwd=3)
  #abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
  abline(v=0, lwd=1,col="black",lty=2)
  mtext("Flowering",side=2,line=6, cex=1.2)
  mtext("G)",side=3,line=1, cex=1.2, adj=0)
  
  #legend("topright", c("Forest","Grassland"), lty = c(1,1), col = c("forestgreen",'goldenrod'), lwd=2,bty="n")
  
  dta_forest <- density(forestt, na.rm = TRUE)
  dta_grassland <- density(grasslandt, na.rm = TRUE)
  
  #moisture
  dta_forestm <- density(forestm, na.rm = TRUE)
  dta_grasslandm <- density(grasslandm, na.rm = TRUE)
  
  plot(dta_forestm, col ="forestgreen", main = "Moisture", 
       xlim=c(-40,40),
       ylim = c(0, max(dta_forestm$y,dta_grasslandm$y)),
       lwd=3,bty="l", xlab="Effect on flowering per SD",
       cex.axis=1.5, cex.lab=1.5)
  lines(dta_grasslandm, col = "goldenrod",lwd=3)
  #abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
  abline(v=0, lwd=1,col="black",lty=2)
  mtext("Flowering",side=2,line=6, cex=1.2)
  mtext("H)",side=3,line=1, cex=1.2, adj=0)
  
  #interaction
  dta_forestint <- density(forestint, na.rm = TRUE)
  dta_grasslandint <- density(grasslandint, na.rm = TRUE)
  
  plot(dta_forestint, col ="forestgreen", main = "Temperature*Moisture", 
       xlim=c(-40,40),
       ylim = c(0, max(dta_forestint$y,dta_grasslandint$y)),
       lwd=3,bty="l", xlab="Effect on flowering per SD",
       cex.axis=1.5, cex.lab=1.5)
  lines(dta_grasslandint, col = "goldenrod",lwd=3)
  #abline(v=fixef(mod)[2,1], lwd=2,col="red", lty=1)
  abline(v=0, lwd=1,col="black",lty=2)
  mtext("Flowering",side=2,line=6, cex=1.2)
  mtext("I)",side=3,line=1, cex=1.2, adj=0)
  
  dev.off()

  #Table for supplement
  #flowering posteriors
  fl_ecosystem_posts<-rbind(
    c("flowering","forest","$\\mu_{temp}$",round(mean(forestt), digits=2),round(median(forestt), digits=2),round(quantile(forestt,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("flowering","forest","$\\mu_{mois}$",round(mean(forestm), digits=2),round(median(forestm), digits=2),round(quantile(forestm,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("flowering","forest","$\\mu_{temp:mois}$",round(mean(forestint), digits=2),round(median(forestint), digits=2),round(quantile(forestint,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("flowering","grassland","$\\mu_{temp}$",round(mean(grasslandt), digits=2),round(median(grasslandt), digits=2),round(quantile(grasslandt,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("flowering","grassland","$\\mu_{mois}$",round(mean(grasslandm), digits=2),round(median(grasslandm), digits=2),round(quantile(grasslandm,probs=c(0.1,.25,.75,.9)), digits=2)),
    c("flowering","grassland","$\\mu_{temp:mois}$",round(mean(grasslandint), digits=2),round(median(grasslandint), digits=2),round(quantile(grasslandint,probs=c(0.1,.25,.75,.9)), digits=2)) 
  )
  
  #table of both ecosystemns across all phenophses
  allphen_ecosystem_posts<-rbind(
    bb_ecosystem_posts,
    lo_ecosystem_posts,
    fl_ecosystem_posts
  )
  colnames(allphen_ecosystem_posts)[1:5]<- c("phenophase","ecosystem","predictor","mean","50%")
  
  write.csv(allphen_ecosystem_posts,"Analyses/soilmoisture/ecosystem_post_table.csv",row.names = FALSE)
  
  
  
  
  
  
  ##using ggplot, just to check...
  
  bbmod<- load("Analyses/output/brms/testm5cent.brms.bb.Rda")
  fit<- testm5cent.brms
  colnames(bbform)[2]<-"sp"
  
library(brms)
  library(tidybayes)
  library(dplyr)
  library(ggplot2)
  
  
  temp_draws <- fit %>%
    spread_draws(r_sp[sp,temp]) %>%
    left_join(bbform, by = "sp")
  temp_draws<-temp_draws[temp_draws$temp=="temp",]
  ggplot(temp_draws,
         aes(x = r_sp,
             colour = form,
             fill = form)) +
    geom_density(alpha = 0.2, linewidth = 1.1) +
    theme_classic() +
    labs(
      x = "Species temperature effect",
      y = "Density",
      colour = "Life form",
      fill = "Life form"
    )
  
  mois_draws <- fit %>%
    spread_draws(r_sp[sp,mois]) %>%
    left_join(bbform, by = "sp")
  mois_draws<-mois_draws[mois_draws$mois=="mois",]
  ggplot(mois_draws,
         aes(x = r_sp,
             colour = form,
             fill = form)) +
    geom_density(alpha = 0.2, linewidth = 1.1) +
    theme_classic() +
    labs(
      x = "Species soil moisture effect",
      y = "Density",
      colour = "Life form",
      fill = "Life form"
    )
  