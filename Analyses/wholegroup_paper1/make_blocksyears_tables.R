#Tables of blocks versus years  
options(stringsAsFactors = FALSE)
library(plyr)
library(dplyr)
library(lme4)
library(car)
library(xtable)
setwd("/Users/aileneettinger/git/radcliffe/Analyses/wholegroup_paper1")
expclim<-read.csv("../../Analyses/expclim.csv", header=TRUE)
exppheno<-read.csv("../../Analyses/exppheno.csv", header=TRUE)
treats<-read.csv("../../Analyses/treats_detail.csv", header=T)

#standard data wrangling to get expclim2 for climate analyses and expgdd for phenology analyses
source("../../Analyses/source/standard_mergesandwrangling.R")
expclimt<-expclim2[which(expclim2$preciptreat==0|is.na(expclim2$preciptreat)),]#select only plots with unmanipulated precip

#try doing the below with target warming instead of temp treat....
blockdat<-expclimt[which(!is.na(expclimt$block)),]#select only sites with blocked design
blockdat<-expclimt[which(expclimt$block!="none"),]#select only sites with blocked design

blockdat<-blockdat[order(blockdat$site,blockdat$block,blockdat$plot,blockdat$year,blockdat$doy),]
blockdat2<-subset(blockdat,select=c(site,block,year,styear,doy,temptreat,target,soiltemp1_mean,agtemp_min,agtemp_max))
blockdat2$block<-as.factor(blockdat2$block)
blockdat2$year<-as.factor(blockdat2$year)
blockdat2$site<-as.factor(blockdat2$site)
blockdat2$styear<-as.factor(blockdat2$styear)
blockdat2$target<-as.numeric(blockdat2$target)

#blockdat2  <- blockdat2 [apply(blockdat2 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#blockdat2<-blockdat2[-which(blockdat2$temptreat=="ambient"),]
#testdat2<-blockdat2[which(as.numeric(as.character(blockdat2$year))<2011),]
#testdat3<-testdat2[which(as.numeric(as.character(testdat2$year))>2008),]

yearmod<-lmer(soiltemp1_mean~target*styear + (1|site/block), data= blockdat2, REML=FALSE)

yearmod2<-lmer(agtemp_min~target*styear + (1|site/block), data= blockdat2, REML=FALSE)
yearmod3<-lmer(agtemp_max~target*styear + (1|site/block), data= blockdat2, REML=FALSE)
yeartable<-Anova(yearmod, type="III");
yeartable2<-Anova(yearmod2, type="III")
yeartable3<-Anova(yearmod3, type="III")
colnames(yeartable)[3]<-colnames(yeartable2)[3]<-colnames(yeartable3)[3]<-"p"
yeartable$p<-as.numeric(yeartable$p)
yeartable2$p<-as.numeric(yeartable2$p)
yeartable3$p<-as.numeric(yeartable3$p)
yeartable$p[yeartable$p>=0.001]<-round(yeartable$p[yeartable$p>=0.001], digits=3)
yeartable2$p[yeartable2$p>=0.001]<-round(yeartable2$p[yeartable2$p>=0.001], digits=3)
yeartable3$p[yeartable3$p>=0.001]<-round(yeartable3$p[yeartable3$p>=0.001], digits=3)
yeartable$p[yeartable$p<0.001]<-"<0.001"
yeartable2$p[yeartable2$p<0.001]<-"<0.001"
yeartable3$p[yeartable3$p<0.001]<-"<0.001"
yeartableAll<-as.data.frame(cbind(rownames(yeartable),yeartable$Df,round(yeartable$Chisq,digits=2),yeartable$p,round(yeartable2$Chisq,digits=2),yeartable2$p,round(yeartable3$Chisq,digits=2),yeartable3$p))
colnames(yeartableAll)<-c("predictor","df","chi2","p","chi2","p","chi2","p")
write.csv(yeartableAll,"../../Analyses/wholegroup_paper1/output/yeartableAll.csv",row.names=FALSE)
yeartableAll<-read.csv("../../Analyses/wholegroup_paper1/output/yeartableAll.csv",header=TRUE)

#testdat<-blockdat2[as.numeric(as.character(blockdat2$block))<4,]
blockmod<-lmer(soiltemp1_mean~target*block + (1|site/styear), data=blockdat2, REML=FALSE)
blockmod2<-lmer(agtemp_min~target*block + (1|site/styear), data=blockdat2, REML=FALSE)
blockmod3<-lmer(agtemp_max~target*block + (1|site/styear), data= blockdat2, REML=FALSE)
blocktable<-Anova(blockmod, type="III")
blocktable2<-Anova(blockmod2, type="III")
blocktable3<-Anova(blockmod3, type="III")
colnames(blocktable)[3]<-colnames(blocktable2)[3]<-colnames(blocktable3)[3]<-"p"
blocktable$p<-as.numeric(blocktable$p)
blocktable2$p<-as.numeric(blocktable2$p)
blocktable3$p<-as.numeric(blocktable3$p)
blocktable$p[blocktable$p>=0.001]<-round(blocktable$p[blocktable$p>=0.001], digits=3)
blocktable2$p[blocktable2$p>=0.001]<-round(blocktable2$p[blocktable2$p>=0.001], digits=3)
blocktable3$p[blocktable3$p>=0.001]<-round(blocktable3$p[blocktable3$p>=0.001], digits=3)
blocktable$p[blocktable$p<0.001]<-"<0.001"
blocktable2$p[blocktable2$p<0.001]<-"<0.001"
blocktable3$p[blocktable3$p<0.001]<-"<0.001"
blocktableAll<-as.data.frame(cbind(rownames(blocktable),blocktable$Df,round(blocktable$Chisq,digits=2),blocktable$p,round(blocktable2$Chisq,digits=2),blocktable2$p,round(blocktable3$Chisq,digits=2),blocktable3$p))
colnames(blocktableAll)<-NULL
blocktableAll[,1]<-c("intercept","temp. treatment","block","temp. treatment:block")
colnames(blocktableAll)<-c("predictor","df","chi2","p","chi2","p","chi2","p")

#blocktableAll<-rbind(c("","","soil mean","temp","above-ground","min temp","above-ground","max temp"),c("predictor","df","$\\chi^2$","p","$\\chi^2$","p","$\\chi^2$","p"),blocktableAll)
write.csv(blocktableAll,"../../Analyses/wholegroup_paper1/output/blocktableAll.csv",row.names=FALSE)
