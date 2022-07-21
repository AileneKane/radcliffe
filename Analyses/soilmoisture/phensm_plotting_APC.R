## Started 22 Jan 2019 ##
## By Ailene ##

## Marginal effects from Stan models ##
## Based off Lizzie's models_stan_plotting_APC.R code for OSPREE ##

############################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#libraries
library(RColorBrewer)
library(dplyr)
library(brms)
library(scales)
# Setting working directory.
  setwd("~/GitHub/radcliffe")

figpath <- "Analyses/soilmoisture/figures"


#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=TRUE)
forms<-read.csv("Analyses/output/lifeform.csv", header=TRUE)
#Make some choices about how to restrict the data and which model to use:
remove.conifers=TRUE
use.airtemp=TRUE
use.centmod=FALSE
#data of interest for plotting (phenophase, site, sp, if applicable)
phen="BB"#options that work are "BB" or "LO"
site=1
#standard data wrangling to get expclim2 for climate analyses and expgdd
#for phenology analyses (with gddcrit)
source("Analyses/source/standard_mergesandwrangling.R")

#summarize climate data by plot (annual and seasonal temp, soil mois), 
#merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
source("Analyses/source/climsum_byplot.R")

#Prepare data for phenology models in stan
source("Analyses/source/stanprep_phenmods.R")


# Set up colors (more than used currently ...)
cols <- adjustcolor(c("darkgray","salmon","darkred","lightskyblue","darkblue"), alpha.f = 0.8) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4

#load budburst model
if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==FALSE & phen=="BB"){
  load("Analyses/output/brms/testm5.brms.bb.Rda")
  mod<-testm5.brms
  fit <- fixef(mod)
  sitetemp<-mean(expgdd_bbd$ag_min_jm[expgdd_bbd$site==site], na.rm=TRUE)#mean spring temp (current)
  sitemois<-mean(expgdd_bbd$soilmois_janmar[expgdd_bbd$site==site], na.rm=TRUE)#mean soil mois (current)
}
if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==TRUE & phen=="BB"){
  load("Analyses/output/brms/testm5cent.brms.bb.Rda")
  mod<-testm5cent.brms
  }

#find species with largest response to moisture and smallest response to moisture
sp<-coef(mod,probs=c(0.1,0.9))$sp
intercepts<-sp[1:41,,1]
moisef<-sp[1:41,,3]
tempef<-sp[1:41,,2]
intef<-sp[1:41,,4]

moissenssp<-sp[which(moisef[,1]==min(moisef[,1])),,]
tempsenssp<-sp[which(tempef[,1]==min(tempef[,1])),,]
#identify species with smallest interaction term
lowintsp<-sp[which(intef[,1]==min(abs(0-intef[,1]))),,]#32 or 135?
maxnegintsp<-sp[which(intef[,1]==min(intef[,1])),,]#32 or 135?

speftab<-as.data.frame(cbind(tempef[,1],moisef[,1]))
colnames(speftab)<-c("temp","mois")
speftab[order(speftab$mois),]
rownameshere <- c("mu_a_sp", "mu_b_temp_sp", "mu_b_mois_sp", "mu_b_tsm_sp")
rownames(fit)[1:length(rownameshere)]<-rownameshere
#For main effects of model:
tempforecast.raw<-seq(0,5, by=0.5)#enter in the amount of warming (in degrees C) you want to forecast 
#centered version of temperature change
#tempforecast.cent<-seq(0,1.6,by=0.16)#not sure how to do this automatically 

smforecast<-seq(-2,2, by=0.5)
drysm<- -0.10#proportion change
dry2sm<- -1
wetsm<- 0.10
wet2sm<- 1
#range(expgdd_bbd$soilmois_janmar)#0.1020346 0.3879270
#mean(expgdd_bbd$soilmois_janmar)#0.2146834
#so 10% reduction in soil moisture would be 
#mean(expgdd_bbd$soilmois_janmar)+(mean(expgdd_bbd$soilmois_janmar)*-0.1)#0.1938242
## Plotting
# First, we estimate the posteriors for each thing we want to plot...

getest.bb <- function(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm, wet2sm){
  listofdraws <-as.data.frame(as.matrix(mod))
  colnames(listofdraws)[1:length(rownameshere)]<-rownameshere
    avgbb <- listofdraws$mu_a_sp + listofdraws$mu_b_temp_sp*temp + 
        listofdraws$mu_b_mois_sp*sm + listofdraws$mu_b_tsm_sp*temp*sm
    warmbb <- listofdraws$mu_a_sp + listofdraws$mu_b_temp_sp*(temp+warmtemp) + 
      listofdraws$mu_b_mois_sp*sm + listofdraws$mu_b_tsm_sp*(temp+warmtemp)*sm
    warmdrybb <- listofdraws$mu_a_sp + listofdraws$mu_b_temp_sp*(temp+warmtemp) + 
        listofdraws$mu_b_mois_sp*(sm+(drysm*sm)) + listofdraws$mu_b_tsm_sp*(temp+warmtemp)*(sm+(drysm*sm))
    warmdry2bb <- listofdraws$mu_a_sp + listofdraws$mu_b_temp_sp*(temp+warmtemp) + 
      listofdraws$mu_b_mois_sp*(sm+(dry2sm*sm)) + listofdraws$mu_b_tsm_sp*(temp+warmtemp)*(sm+(dry2sm*sm))
    warmwetbb <- listofdraws$mu_a_sp + listofdraws$mu_b_temp_sp*(temp+warmtemp) + 
      listofdraws$mu_b_mois_sp*(sm+(wetsm*sm)) + listofdraws$mu_b_tsm_sp*(temp+warmtemp)*(sm+(wetsm*sm))
    warmwet2bb <- listofdraws$mu_a_sp + listofdraws$mu_b_temp_sp*(temp+warmtemp) + 
      listofdraws$mu_b_mois_sp*(sm+(wet2sm*sm)) + listofdraws$mu_b_tsm_sp*(temp+warmtemp)*(sm+(wet2sm*sm))
    
    yebbest <- list(avgbb, warmbb, warmdrybb,warmdry2bb,warmwetbb,warmwet2bb)
    return(yebbest)
}

#make blank dataframe to fill with estimates
predicts <- as.data.frame(matrix(NA,ncol=7,nrow=length(tempforecast.raw)))

predicts.10per <- as.data.frame(matrix(NA,ncol=7,nrow=length(tempforecast.raw)))
predicts.90per <- as.data.frame(matrix(NA,ncol=7,nrow=length(tempforecast.raw)))
colnames(predicts)<-colnames(predicts.10per) <-colnames(predicts.90per) <-
  c("amt.warming","bb.nowarm","bb.warm","warm.dry","warm.dry2","warm.wet","warm.wet2")

for (i in 1:length(tempforecast.raw)){
  temp<-sitetemp
  warmtemp <-tempforecast.raw[i]
  sm <-  sitemois
  drysm<-drysm
  bbposteriors <- getest.bb(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm,wet2sm)
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.10, 0.5, 0.90)))
  quant10per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.1))))
  quant90per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.9))))
   predicts[i,]<-c(warmtemp,meanz)
  predicts.10per[i,]<-c(warmtemp,quant10per)
  predicts.90per[i,]<-c(warmtemp,quant90per)
}

predictsbb<-predicts
predictsbb<-predictsbb[,-2]
predictsbb.10per<-predicts.10per
predictsbb.10per<-predictsbb.10per[,-2]
predictsbb.90per<-predicts.90per
predictsbb.90per<-predictsbb.90per[,-2]
xlim = c(range(tempforecast.raw))
ylim = c(min(round(predictsbb.10per[,-1], digits=0)), max(round(predictsbb.90per[,-1], digits=0)))


figname<-paste("tempforecast","bb",min(tempforecast.raw),max(tempforecast.raw),"degwarm.pdf", sep="_")
pdf(file.path(figpath,figname), width = 9, height = 6)

#pdf(paste(figpath,"/tempforecast",min(tempforecast),"-",max(tempforecast),"_deg_",lat,"_",long,".pdf",sep=""))
#quartz()
#windows()
par(mar=c(8,7,3,5))
plot(x=NULL,y=NULL, xlim=xlim, xaxt="n",xlab="Amount of warming (C)", ylim=ylim,
     ylab="Days to BB", main="BB", bty="l")
   #Add shading around line for credible intervals
  
  for(i in 3:7){
  polygon(c(rev(predictsbb$amt.warming), predictsbb$amt.warming), c(rev(predictsbb.90per[,i-1]), predictsbb.10per[,i-1]), col = alpha(cols[i-2], 0.2), border = NA)
  }
  #i=3
  #i=5
  for(i in 3:7){
    lines(predictsbb$amt.warming, predictsbb[,i-1], 
          col=cols[i-2], lwd=2)
    }
  #i=3
  
  #axis(side=1,at=c(0,0.32,0.64,0.96,1.28,1.60), labels=c(0,1,2,3,4,5))
  axis(side=1,at=c(0,1,2,3,4,5), labels=c(0,1,2,3,4,5))
  
  # intervals
  # for(i in 3:5){
  #   lines(predicts.25per$warming, predicts.25per[,i-1], 
  #         col=cols[i-2], lwd=1, lty=2)
  # }
  # for(i in 3:5){
  #   lines(predicts.75per$warming, predicts.75per[,i-1], 
  #         col=cols[i-2], lwd=1, lty=2)
  # }
legend(3,120,legend=c("Warming only","-10% Drier","-100% Drier","+10% Wetter","100% Wetter"),lty=1,lwd=2,col=cols,bty="n", cex=0.9)
dev.off()

#Make version of figure with main effects and 3 species, and accounting for site effects too

getest.bb.sp <- function(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm, wet2sm,spnum,sitenum){
  listofdraws <-as.data.frame(as.matrix(mod))
  int_sp<-which(colnames(listofdraws)==paste("r_sp[",spnum,",Intercept]", sep=""))
  int_site<-which(colnames(listofdraws)==paste("r_site[",sitenum,",Intercept]", sep=""))
  b_temp_sp<-which(colnames(listofdraws)==paste("r_sp[",spnum,",temp]", sep=""))
  b_mois_sp<-which(colnames(listofdraws)==paste("r_sp[",spnum,",mois]", sep=""))
  b_tsm_sp<-which(colnames(listofdraws)==paste("r_sp[",spnum,",temp:mois]", sep=""))
  
  spavgbb <- listofdraws[,int_sp] + listofdraws[,int_site] + listofdraws$b_Intercept + (listofdraws[,b_temp_sp]+listofdraws$b_temp)*temp + 
    (listofdraws[,b_mois_sp]+listofdraws$b_mois)*sm + (listofdraws[,b_tsm_sp]+listofdraws$`b_temp:mois`)*temp*sm
  
  avgbb <- listofdraws$mu_a_sp + listofdraws[,int_site]+ listofdraws$b_Intercept+ (listofdraws[,b_temp_sp]+listofdraws$b_temp)*temp + 
    (listofdraws[,b_mois_sp]+listofdraws$b_mois)*sm + (listofdraws[,b_tsm_sp]+listofdraws$`b_temp:mois`)*temp*sm
  
  spwarmbb <- listofdraws[,int_sp] + listofdraws[,int_site]+ listofdraws$b_Intercept+ (listofdraws[,b_temp_sp]+listofdraws$b_temp)*(temp+warmtemp) + 
    (listofdraws[,b_mois_sp]+listofdraws$b_mois)*sm + (listofdraws[,b_tsm_sp]+listofdraws$`b_temp:mois`)*(temp+warmtemp)*sm
  
  spwarmdrybb <-listofdraws[,int_sp]+ listofdraws[,int_site] + listofdraws$b_Intercept + (listofdraws[,b_temp_sp]+listofdraws$b_temp)*(temp+warmtemp) + 
    (listofdraws[,b_mois_sp]+listofdraws$b_mois)*(sm+(drysm*sm)) + (listofdraws[,b_tsm_sp]+listofdraws$`b_temp:mois`)*(temp+warmtemp)*(sm+(drysm*sm))
  
  spwarmdry2bb <- listofdraws[,int_sp] + listofdraws[,int_site]+ listofdraws$b_Intercept + (listofdraws[,b_temp_sp]+listofdraws$b_temp)*(temp+warmtemp) + 
    (listofdraws[,b_mois_sp]+listofdraws$b_mois)*(sm+(dry2sm*sm)) + (listofdraws[,b_tsm_sp]+listofdraws$`b_temp:mois`)*(temp+warmtemp)*(sm+(dry2sm*sm))
  
  spwarmwetbb <- listofdraws[,int_sp] + listofdraws[,int_site]+ listofdraws$b_Intercept + (listofdraws[,b_temp_sp]+listofdraws$b_temp)*(temp+warmtemp) + 
    (listofdraws[,b_mois_sp]+listofdraws$b_mois)*(sm+(wetsm*sm)) + (listofdraws[,b_tsm_sp]+listofdraws$`b_temp:mois`)*(temp+warmtemp)*(sm+(wetsm*sm))
  
  spwarmwet2bb <- listofdraws[,int_sp] + listofdraws[,int_site]+ listofdraws$b_Intercept + (listofdraws[,b_temp_sp]+listofdraws$b_temp)*(temp+warmtemp) + 
    (listofdraws[,b_mois_sp]+listofdraws$b_mois)*(sm+(wet2sm*sm)) + (listofdraws[,b_tsm_sp]+listofdraws$`b_temp:mois`)*(temp+warmtemp)*(sm+(wet2sm*sm))
  
  yebbest <- list(spavgbb, spwarmbb, spwarmdrybb,spwarmdry2bb,spwarmwetbb,spwarmwet2bb)
  return(yebbest)
}

#make blank dataframe to fill with estimates
predicts <- as.data.frame(matrix(NA,ncol=7,nrow=length(tempforecast.raw)))

predicts.10per <- as.data.frame(matrix(NA,ncol=7,nrow=length(tempforecast.raw)))
predicts.90per <- as.data.frame(matrix(NA,ncol=7,nrow=length(tempforecast.raw)))
colnames(predicts)<-colnames(predicts.10per) <-colnames(predicts.90per) <-
  c("amt.warming","bb.nowarm","bb.warm","warm.dry","warm.dry2","warm.wet","warm.wet2")

splegbb<- expgdd_bbd %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)
splegbb<-splegbb[order(splegbb$genus.species),]
colnames(splegbb)[2]<-"spnumbb"
table(expgdd_bbd$site)
table(expgdd_bbd$site,expgdd_bbd$genus.species)

spnum<-c(28,105,135)
#choose:
#3:Acer rubrum: +inxn, - effects of both moisture and temp
#105:Nyssa sylvatica: +inxn, - effects of both moisture and temp
#28: Carya glabra, -inxn, - effects of both moisture and temp
#Or 22: Betula, -inxn, - effects of both moisture and temp
#135 = Quercus nigra,  inxn term close to 0, wide incertainty intervales effects of both moisture and temp
sitenum = 4
spname<-splegbb$sp.name[match(spnum,splegbb$spnumb)]

#which sites are associated with each species?
unique(expgdd_bbd$site[expgdd_bbd$genus.species==28])#3,4
unique(expgdd_bbd$site[expgdd_bbd$genus.species==3])#1,3,4,5,7
unique(expgdd_bbd$site[expgdd_bbd$genus.species==105])#3,4
unique(expgdd_bbd$site[expgdd_bbd$genus.species==135])#3,4


#spnum<-splegbb$spnumbb[25:36]
#spname<-splegbb$sp.name[match(spnum,splegbb$spnumb)]
figname<-paste("tempforecast","bb",min(tempforecast.raw),max(tempforecast.raw),spnum[1],spnum[2],spnum[3],sitenum,"degwarm.pdf", sep="_")
pdf(file.path(figpath,figname), width = 12, height = 4)
#quartz()
par(mar=c(8,7,3,5),mfrow=c(1,4))
plot(x=NULL,y=NULL, xlim=xlim, xaxt="n",xlab="Amount of warming (C)", ylim=ylim,
     ylab="Days to BB", bty="l")
#Add shading around line for credible intervals

for(i in c(3,5,7)){
  polygon(c(rev(predictsbb$amt.warming), predictsbb$amt.warming), c(rev(predictsbb.90per[,i-1]), predictsbb.10per[,i-1]), col = alpha(cols[i-2], 0.2), border = NA)
}
#i=3
#i=5
for(i in c(3,5,7)){
  lines(predictsbb$amt.warming, predictsbb[,i-1], 
        col=cols[i-2], lwd=2)
}
#i=3

#axis(side=1,at=c(0,0.32,0.64,0.96,1.28,1.60), labels=c(0,1,2,3,4,5))
axis(side=1,at=c(0,1,2,3,4,5), labels=c(0,1,2,3,4,5))
mtext("A)", side=3, line =1,adj=-.3)
# intervals
# for(i in 3:5){
#   lines(predicts.25per$warming, predicts.25per[,i-1], 
#         col=cols[i-2], lwd=1, lty=2)
# }
# for(i in 3:5){
#   lines(predicts.75per$warming, predicts.75per[,i-1], 
#         col=cols[i-2], lwd=1, lty=2)
# }
legend("bottomleft",legend=c("Warming only","-100% Drier soil","100% Wetter soil"),lty=1,lwd=2,col=c(cols[1],cols[3],cols[5]),bty="n", cex=0.9)
let<-c("B)","C)","D)")
for(s in 1:length(spnum)){
for (i in 1:length(tempforecast.raw)){
  temp<-sitetemp
  warmtemp <-tempforecast.raw[i]
  sm <-  sitemois
  drysm<-drysm
  bbposteriors <- getest.bb.sp(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm,wet2sm,spnum[s],sitenum)
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.10, 0.5, 0.90)))
  quant10per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.1))))
  quant90per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.9))))
  predicts[i,]<-c(warmtemp,meanz)
  predicts.10per[i,]<-c(warmtemp,quant10per)
  predicts.90per[i,]<-c(warmtemp,quant90per)
}

predictsbb<-predicts
predictsbb<-predictsbb[,-2]
predictsbb.10per<-predicts.10per
predictsbb.10per<-predictsbb.10per[,-2]
predictsbb.90per<-predicts.90per
predictsbb.90per<-predictsbb.90per[,-2]
xlim = c(range(tempforecast.raw))
ylim = c(min(round(predictsbb.10per[,-1], digits=0)), max(round(predictsbb.90per[,-1], digits=0)))

plot(x=NULL,y=NULL, xlim=xlim, xaxt="n",xlab="Amount of warming (C)", ylim=ylim,
     ylab="Budburst Day", main=paste(spname[s],sep=","), bty="l")

#Add shading around line for credible intervals

for(j in c(3,5,7)){
  polygon(c(rev(predictsbb$amt.warming), predictsbb$amt.warming), c(rev(predictsbb.90per[,j-1]), predictsbb.10per[,j-1]), col = alpha(cols[j-2], 0.2), border = NA)
}

for(j in c(3,5,7)){
  lines(predictsbb$amt.warming, predictsbb[,j-1], 
        col=cols[j-2], lwd=2)
}

mtext(paste(let[s]), side=3, line =1,adj=-.3)

#axis(side=1,at=c(0,0.32,0.64,0.96,1.28,1.60), labels=c(0,1,2,3,4,5))
axis(side=1,at=c(0,1,2,3,4,5), labels=c(0,1,2,3,4,5))

# intervals
# for(i in 3:5){
#   lines(predicts.25per$warming, predicts.25per[,i-1], 
#         col=cols[i-2], lwd=1, lty=2)
# }
# for(i in 3:5){
#   lines(predicts.75per$warming, predicts.75per[,i-1], 
#         col=cols[i-2], lwd=1, lty=2)
# }


#if(s==1){legend("bottomleft",legend=c("Warming only","-100% Drier soil","100% Wetter soil"),lty=1,lwd=2,col=c(cols[1],cols[3],cols[5]),bty="n", cex=0.9)}

}
dev.off()


#make blank dataframe to fill with estimates
predicts <- as.data.frame(matrix(NA,ncol=7,nrow=length(tempforecast.raw)))

predicts.10per <- as.data.frame(matrix(NA,ncol=7,nrow=length(tempforecast.raw)))
predicts.90per <- as.data.frame(matrix(NA,ncol=7,nrow=length(tempforecast.raw)))
colnames(predicts)<-colnames(predicts.10per) <-colnames(predicts.90per) <-
  c("amt.warming","bb.nowarm","bb.warm","warm.dry","warm.dry2","warm.wet","warm.wet2")
spnum<-c(104,25)
for (i in 1:length(tempforecast.raw)){
  temp<-sitetemp
  warmtemp <-tempforecast.raw[i]
  sm <-  sitemois
  drysm<-drysm
  bbposteriors <- getest.bb.sp(mod, temp, sm, warmtemp, drysm,dry2sm,wetsm,wet2sm,spnum[2])
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.10, 0.5, 0.90)))
  quant10per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.1))))
  quant90per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.9))))
  predicts[i,]<-c(warmtemp,meanz)
  predicts.10per[i,]<-c(warmtemp,quant10per)
  predicts.90per[i,]<-c(warmtemp,quant90per)
}

predictsbb<-predicts
predictsbb<-predictsbb[,-2]
predictsbb.10per<-predicts.10per
predictsbb.10per<-predictsbb.10per[,-2]
predictsbb.90per<-predicts.90per
predictsbb.90per<-predictsbb.90per[,-2]
xlim = c(range(tempforecast.raw))
ylim = c(min(round(predictsbb.10per[,-1], digits=0)), max(round(predictsbb.90per[,-1], digits=0)))



dev.off()

figname<-paste("tempforecast","bb",min(tempforecast.raw),max(tempforecast.raw),"degwarm.pdf", sep="_")
pdf(file.path(figpath,figname), width = 9, height = 6)

#pdf(paste(figpath,"/tempforecast",min(tempforecast),"-",max(tempforecast),"_deg_",lat,"_",long,".pdf",sep=""))
#quartz()
par(mar=c(8,7,3,5), mfrow=c(1,2))
plot(x=NULL,y=NULL, xlim=xlim, xaxt="n",xlab="Amount of warming (C)", ylim=ylim,
     ylab="Days to BB", main="BB", bty="l")
#Add shading around line for credible intervals

for(i in 3:7){
  polygon(c(rev(predictsbb$amt.warming), predictsbb$amt.warming), c(rev(predictsbb.90per[,i-1]), predictsbb.10per[,i-1]), col = alpha(cols[i-2], 0.2), border = NA)
}
#i=3
#i=5
for(i in 3:7){
  lines(predictsbb$amt.warming, predictsbb[,i-1], 
        col=cols[i-2], lwd=2)
}
#i=3
lines(predictsbb$amt.warming, predictsbb[,i-1], 
      col=cols[i-2], lwd=3)
#axis(side=1,at=c(0,0.32,0.64,0.96,1.28,1.60), labels=c(0,1,2,3,4,5))
axis(side=1,at=c(0,1,2,3,4,5), labels=c(0,1,2,3,4,5))

# intervals
# for(i in 3:5){
#   lines(predicts.25per$warming, predicts.25per[,i-1], 
#         col=cols[i-2], lwd=1, lty=2)
# }
# for(i in 3:5){
#   lines(predicts.75per$warming, predicts.75per[,i-1], 
#         col=cols[i-2], lwd=1, lty=2)
# }
legend(3,120,legend=c("Warming only","-10% Drier","-100% Drier","+10% Wetter","100% Wetter"),lty=1,lwd=2,col=cols,bty="n", cex=0.9)
dev.off()



#Now look at effect of soil moisture on the x axis, at different temperatures, using the main model
temps<-c(quantile(expgdd_bbd$ag_min_janmar))#enter in the amount of warming (in degrees C) you want to forecast 

range(expgdd_bbd$ag_min_janmar)
range(expgdd_bbd$soilmois_janmar)

sm<-seq(from=0, to=0.4, by=0.05)
getest.bb <- function(mod, temp, sm, cold2temp, cold1temp, warm1temp, warm2temp){
  listofdraws <-as.data.frame(as.matrix(mod))
  colnames(listofdraws)[1:length(rownameshere)]<-rownameshere
  avgbb <- listofdraws$mu_a_sp + listofdraws$mu_b_temp_sp*temp + 
    listofdraws$mu_b_mois_sp*sm + listofdraws$mu_b_tsm_sp*temp*sm
   warm1bb <- listofdraws$mu_a_sp + listofdraws$mu_b_temp_sp*(warm1temp) + 
    listofdraws$mu_b_mois_sp*sm + listofdraws$mu_b_tsm_sp*(warm1temp)*sm
  warm2bb <- listofdraws$mu_a_sp + listofdraws$mu_b_temp_sp*(warm2temp) + 
    listofdraws$mu_b_mois_sp*sm + listofdraws$mu_b_tsm_sp*(warm2temp)*sm
  cold1bb <- listofdraws$mu_a_sp + listofdraws$mu_b_temp_sp*(cold1temp) + 
    listofdraws$mu_b_mois_sp*sm + listofdraws$mu_b_tsm_sp*(cold1temp)*sm
  cold2bb <- listofdraws$mu_a_sp + listofdraws$mu_b_temp_sp*(cold2temp) + 
    listofdraws$mu_b_mois_sp*sm + listofdraws$mu_b_tsm_sp*(cold2temp)*sm
  
  yebbest <- list(sm,cold2bb,cold1bb,avgbb, warm1bb, warm2bb)
  return(yebbest)
}

#make blank dataframe to fill with estimates
predicts <- as.data.frame(matrix(NA,ncol=6,nrow=length(temps)))

predicts.loper <- as.data.frame(matrix(NA,ncol=6,nrow=length(temps)))
predicts.hiper <- as.data.frame(matrix(NA,ncol=6,nrow=length(temps)))
colnames(predicts)<-colnames(predicts.loper) <-colnames(predicts.hiper) <-
  c("sm","bb.cold2","bb.cold1","bb.mean","bb.warm1","bb.warm2")
for (i in 1:length(sm)){  
  temp<-mean(expgdd_bbd$ag_min_janmar)
  cold2temp<-temps[1]
  cold1temp<-temps[2]
  warm1temp<-temps[4]
  warm2temp<-temps[5]
  alltemps<-c(temp,warm1temp,warm2temp,cold1temp,cold2temp)
  bbposteriors <- getest.bb(mod, temp, sm[i], cold2temp, cold1temp, warm1temp, warm2temp)
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.25, 0.5, 0.75)))
  quantlo <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.25))))
  quanthi <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.75))))
  predicts[i,]<-meanz
  predicts.loper[i,]<-quantlo
  predicts.hiper[i,]<-quanthi
}
xlim = c(range(sm))
ylim = c(min(round(predicts.loper[,-1], digits=0)), max(round(predicts.hiper[,-1], digits=0)))



figname<-paste("tempforecast","bb",min(sm),max(sm),"vwc.pdf", sep="_")
pdf(file.path(figpath,figname), width = 9, height = 6)

#pdf(paste(figpath,"/tempforecast",min(tempforecast),"-",max(tempforecast),"_deg_",lat,"_",long,".pdf",sep=""))
#quartz()
cols <- adjustcolor(c("darkblue","lightskyblue","gray","salmon","darkred"), alpha.f = 0.8) 
use<-c(1,3,5)
par(mar=c(8,7,3,5))
plot(x=NULL,y=NULL, xlim=xlim, xaxt="n",xlab="Soil moisture (VWC)", ylim=ylim,
     ylab="Days to BB", main="BB", bty="l")
#Add shading around line for credible intervals

#for(i in 3:7){
#  polygon(c(rev(predicts$sm), predicts$sm), c(rev(predicts.hiper[,i]), predicts.loper[,i]), col = alpha(cols[i-1], 0.2), border = NA)
#}
#i=3
#i=5

for(i in use){
  lines(predicts$sm, predicts[,i+1], 
        col=cols[i], lwd=2)
}

axis(side=1,at=c(0,0.1,0.2,0.3,0.4), labels=c(0,0.1,0.2,0.3,0.4))
#axis(side=1,at=c(0,1,2,3,4,5), labels=c(0,1,2,3,4,5))

# intervals
 for(i in use){
   lines(predicts.loper$sm, predicts.loper[,i+1], 
         col=cols[i], lwd=1, lty=2)
 }
 for(i in use){
   lines(predicts.hiper$sm, predicts.hiper[,i+1], 
         col=cols[i], lwd=1, lty=2)
 }
legend(.3,140,legend=round(c(cold2temp,temp,warm2temp), digits=0),lty=1,lwd=2,col=cols[use],bty="n", cex=0.9)
mtext("Mean temp (C)", side=3,line=-3,adj=1)
dev.off()




#Make same plot for first flower
#load budburst model
if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==TRUE & phen=="FF"){
  load("Analyses/output/brms/testm5cent.brms.ff.Rda")
  mod<-testm5cent.ffd.brms
  fit <- testm5cent.ffd.brms$stan_summary
  sitetemp<-mean(expgdd_ffd$ag_min_aprjun [expgdd_ffd$site==site], na.rm=TRUE)#mean spring temp (current)
  sitemois<-mean(expgdd_ffd$soilmois_aprjun[expgdd_ffd$site==site], na.rm=TRUE)#mean soil mois (current)
  temps<-c(quantile(expgdd_ffd$ag_min_janmar))#enter in the amount of warming (in degrees C) you want to forecast 
  
  }

#make blank dataframe to fill with estimates
predicts <- as.data.frame(matrix(NA,ncol=6,nrow=length(temps)))

predicts.loper <- as.data.frame(matrix(NA,ncol=6,nrow=length(temps)))
predicts.hiper <- as.data.frame(matrix(NA,ncol=6,nrow=length(temps)))
colnames(predicts)<-colnames(predicts.loper) <-colnames(predicts.hiper) <-
  c("sm","bb.cold2","bb.cold1","bb.mean","bb.warm1","bb.warm2")
for (i in 1:length(sm)){  
  temp<-mean(expgdd_ffd$ag_min_aprjun)
  cold2temp<-temps[1]
  cold1temp<-temps[2]
  warm1temp<-temps[4]
  warm2temp<-temps[5]
  alltemps<-c(temp,warm1temp,warm2temp,cold1temp,cold2temp)
  bbposteriors <- getest.bb(testm5.rstan, temp, sm[i], cold2temp, cold1temp, warm1temp, warm2temp)
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.25, 0.5, 0.75)))
  quantlo <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.25))))
  quanthi <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.75))))
  predicts[i,]<-meanz
  predicts.loper[i,]<-quantlo
  predicts.hiper[i,]<-quanthi
}
xlim = c(range(sm))
ylim = c(min(round(predicts.loper[,-1], digits=0)), max(round(predicts.hiper[,-1], digits=0)))


figname<-paste("tempforecast","bb",min(sm),max(sm),"vwc.pdf", sep="_")
pdf(file.path(figpath,figname), width = 9, height = 6)

#pdf(paste(figpath,"/tempforecast",min(tempforecast),"-",max(tempforecast),"_deg_",lat,"_",long,".pdf",sep=""))
#quartz()
cols <- adjustcolor(c("darkblue","lightskyblue","gray","salmon","darkred"), alpha.f = 0.8) 
use<-c(1,3,5)
par(mar=c(8,7,3,5))
plot(x=NULL,y=NULL, xlim=xlim, xaxt="n",xlab="Soil moisture (VWC)", ylim=ylim,
     ylab="Days to BB", main="BB", bty="l")
#Add shading around line for credible intervals

#for(i in 3:7){
#  polygon(c(rev(predicts$sm), predicts$sm), c(rev(predicts.hiper[,i]), predicts.loper[,i]), col = alpha(cols[i-1], 0.2), border = NA)
#}
#i=3
#i=5

for(i in use){
  lines(predicts$sm, predicts[,i+1], 
        col=cols[i], lwd=2)
}

axis(side=1,at=c(0,0.1,0.2,0.3,0.4), labels=c(0,0.1,0.2,0.3,0.4))
#axis(side=1,at=c(0,1,2,3,4,5), labels=c(0,1,2,3,4,5))

# intervals
for(i in use){
  lines(predicts.loper$sm, predicts.loper[,i+1], 
        col=cols[i], lwd=1, lty=2)
}
for(i in use){
  lines(predicts.hiper$sm, predicts.hiper[,i+1], 
        col=cols[i], lwd=1, lty=2)
}
legend(.3,140,legend=round(c(cold2temp,temp,warm2temp), digits=0),lty=1,lwd=2,col=cols[use],bty="n", cex=0.9)
mtext("Mean temp (C)", side=3,line=-3,adj)
dev.off()



