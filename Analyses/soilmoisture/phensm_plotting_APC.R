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
# Setting working directory.
  setwd("~/Documents/Github/radcliffe/")

figpath <- "Analyses/soilmoisture/figures"


#Read in experimental climate and phenology data
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("Analyses/treats_detail.csv", header=T)
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
cols <- adjustcolor(c("gray","salmon","darkred","lightskyblue","darkblue"), alpha.f = 0.8) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4

#load budburst model
if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==FALSE & phen=="BB"){
  load("Analyses/output/brms/testm5.rstanarm.bb.Rda")
  mod<-testm5.rstan
  fit <- testm5.rstan$stan_summary
  sitetemp<-mean(expgdd_bbd$ag_min_jm[expgdd_bbd$site==site], na.rm=TRUE)#mean spring temp (current)
  sitemois<-mean(expgdd_bbd$soilmois_janmar[expgdd_bbd$site==site], na.rm=TRUE)#mean soil mois (current)
}
if(remove.conifers==TRUE & use.airtemp==TRUE & use.centmod==TRUE & phen=="BB"){
  load("Analyses/output/brms/testm5.rstanarm.bbcent.Rda")
  mod<-testm5cent.rstan}


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
#range(mean(expgdd_bbd$soilmois_janmar))#0.1020346 0.3879270
#mean(expgdd_bbd$soilmois_janmar)#0.2146834
#so 10% reduction in soil moisture would be 
#mean(expgdd_bbd$soilmois_janmar)+(mean(expgdd_bbd$soilmois_janmar)*-0.1)
## Plotting
# First, we estimate the posteriors for each thing we want to plot...

#list_of_draws <-as.data.frame(as.matrix(testm5cent.rstan))



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

predicts.25per <- as.data.frame(matrix(NA,ncol=7,nrow=length(tempforecast.raw)))
predicts.75per <- as.data.frame(matrix(NA,ncol=7,nrow=length(tempforecast.raw)))
colnames(predicts)<-colnames(predicts.25per) <-colnames(predicts.75per) <-
  c("amt.warming","bb.nowarm","bb.warm","warm.dry","warm.dry2","warm.wet","warm.wet2")

for (i in 1:length(tempforecast.raw)){
  temp<-sitetemp
  warmtemp <-tempforecast.raw[i]
  sm <-  sitemois
  drysm<-drysm
  bbposteriors <- getest.bb(testm5.rstan, temp, sm, warmtemp, drysm,dry2sm,wetsm,wet2sm)
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.25, 0.5, 0.75)))
  quant25per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.25))))
  quant75per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.75))))
   predicts[i,]<-c(warmtemp,meanz)
  predicts.25per[i,]<-c(warmtemp,quant25per)
  predicts.75per[i,]<-c(warmtemp,quant75per)
}

predictsbb<-predicts
predictsbb<-predictsbb[,-2]
predictsbb.25per<-predicts.25per
predictsbb.25per<-predictsbb.25per[,-2]
predictsbb.75per<-predicts.75per
predictsbb.75per<-predictsbb.75per[,-2]
xlim = c(range(tempforecast.raw))
ylim = c(min(round(predictsbb.25per[,-1], digits=0)), max(round(predictsbb.75per[,-1], digits=0)))

figname<-paste("tempforecast","bb",min(tempforecast.raw),max(tempforecast.raw),"degwarm.pdf", sep="_")
pdf(file.path(figpath,figname), width = 9, height = 6)

#pdf(paste(figpath,"/tempforecast",min(tempforecast),"-",max(tempforecast),"_deg_",lat,"_",long,".pdf",sep=""))
#quartz()
par(mar=c(8,7,3,5))
plot(x=NULL,y=NULL, xlim=xlim, xaxt="n",xlab="Amount of warming (C)", ylim=ylim,
     ylab="Days to BB", main="BB", bty="l")
   #Add shading around line for credible intervals
  
  for(i in 3:7){
  polygon(c(rev(predictsbb$amt.warming), predictsbb$amt.warming), c(rev(predictsbb.75per[,i-1]), predictsbb.25per[,i-1]), col = alpha(cols[i-2], 0.2), border = NA)
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
#Just check a few things
