

  expclim<-read.csv("expclim.csv", header=T)
  treats<-read.csv("treats_detail.csv", header=T)
  expclim2<-full_join(treats,expclim, by=c("site", "block", "plot","temptreat","preciptreat"), match="all")
  
  expclimt<-expclim2[which(expclim2$preciptreat==0|is.na(expclim2$preciptreat)),]#select only plots with unmanipulated precip
  expclimt$agtemp_min<-expclimt$airtemp_min
  expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$cantemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$airtemp_min) & !is.na(expclimt$cantemp_min)),]$cantemp_min
  expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$surftemp_min
  expclimt$agtemp_max<-expclimt$airtemp_max
  expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$cantemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$airtemp_max) & !is.na(expclimt$cantemp_max)),]$cantemp_max
  expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$surftemp_max
  expclimt$agtemp_mean<-(expclimt$agtemp_min+expclimt$agtemp_max)/2
  
  #Block and Year varation Analyses for figure showing observed vs target warming by year
  blockdat<-expclimt[which(!is.na(expclimt$block)),]#select only sites with blocked design
  #ok, try looking at differences in a few different ways:
  #First, get daily mean agtemp for each plot, find difference by block
  blockdat<-blockdat[order(blockdat$site,blockdat$block,blockdat$plot,blockdat$year,blockdat$doy),]
  blockdat2<-subset(blockdat,select=c(site,block,plot,year,doy,temptreat,target,soiltemp1_mean,agtemp_mean))
  #add column with relevent control plot
  temps_mean_cont<-blockdat2[blockdat2$temptreat==0,]
  temps_mean_ambcont<-blockdat2[blockdat2$temptreat=="ambient",]
  
  temps_mean_cont<-subset(temps_mean_cont,select=c(site,block,year,doy,soiltemp1_mean,agtemp_mean))
  temps_mean_ambcont<-subset(temps_mean_ambcont,select=c(site,block,year,doy,soiltemp1_mean,agtemp_mean))
  temps_mean_cont<-temps_mean_cont[!temps_mean_cont$site=="exp08",]#remove exp08, because there are also ambient controls (use those to calculate difference)
  temps_mean_cont2<-rbind(temps_mean_cont,temps_mean_ambcont)
  colnames(temps_mean_ambcont)[5:6]<-c("soiltemp1_mean_cont","ag_mean_cont")
  colnames(temps_mean_cont)[5:6]<-c("soiltemp1_mean_cont","ag_mean_cont")
  bdat<-full_join(temps_mean_cont,blockdat2, by=c("site", "block","year","doy"), match="all")
  bdat2<-full_join(temps_mean_ambcont,bdat, by=c("site", "block","year","doy"), match="all")
  bdat$agtemp_dif<-bdat$agtemp_mean-bdat$ag_mean_cont
  bdat$soiltemp_dif<-bdat$soiltemp1_mean-bdat$soiltemp1_mean_cont
  agblockmns<-tapply(bdat$agtemp_dif,list(bdat$block,bdat$target,bdat$site), mean, na.rm=TRUE)
  
  #plot difference between warmed and control, by block and year
  quartz(height=6.5,width=6.5)
  par(mfrow=c(2,2),mai=c(.5,.7,.2,.01),omi=c(.7,.3,.2,.7))
  plot(c(agblockmns[,,1]),pch=rep(c(21,24,23), times=6),col="black",bg="black",xlab="",ylab="Above-ground", bty="l", main="By Block", xlim=c(0,6), ylim=c(0,6), cex.axis=1.2,cex.lab=1.3)
  points(c(agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2),c(agwarm2[,1],agwarm2[,2],agwarm2[,3],agwarm2[,4]),pch=rep(c(21,24,23), times=6),col="black",bg="black")
  points(c(agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3),c(agwarm3[,1],agwarm3[,2],agwarm3[,3],agwarm3[,4]),pch=rep(c(21,24,23), times=6),col="black",bg="black")
  abline(a=0,b=1,lty=1)
  plot(c(agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1,agtarget1$temptreat_1),c(agyearwarm1[,1],agyearwarm1[,2],agyearwarm1[,3],agyearwarm1[,4],agyearwarm1[,5],agyearwarm1[,6]),pch=rep(c(21,24,23), times=5),col="gray",bg="gray",xlab="",ylab="", bty="l", main="By Year",xlim=c(0,6), ylim=c(0,6), cex.axis=1.2,cex.lab=1.2)
  points(c(agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2,agtarget1$temptreat_2),c(agyearwarm2[,1],agyearwarm2[,2],agyearwarm2[,3],agyearwarm2[,4]),pch=rep(c(21,24,23),times=4),col="gray",bg="gray")
  points(c(agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3,agtarget1$temptreat_3),c(agyearwarm3[,1],agyearwarm3[,2],agyearwarm3[,3],agyearwarm3[,4]),pch=rep(c(21,24,23),times=4),col="gray",bg="gray")
  abline(a=0,b=1,lty=1)
  plot(c(rep(target1$temptreat_1,times=6)),c(warm1[,1],warm1[,2],warm1[,3],warm1[,4],warm1[,5],warm1[,6]),pch=rep(c(21,22,24,23), times=6),col="black",bg=c("black","black","black"),xlab="Target warming (C)", ylab="Soil", bty="l", xlim=c(0,6), ylim=c(0,6), cex.axis=1.2,cex.lab=1.2)
  points(c(rep(target1$temptreat_2,times=6)),c(warm2[,1],warm2[,2],warm2[,3],warm2[,4],warm2[,5],warm2[,6]),pch=21,col="black",bg=c("black","black","black"))
  points(c(rep(target1$temptreat_3,times=6)),c(warm3[,1],warm3[,2],warm3[,3],warm3[,4],warm3[,5],warm3[,6]),pch=21,col="black",bg=c("black","black","black"))
  abline(a=0,b=1,lty=1)
  mtext("Observed warming (C)", side=2,line=4.5,adj=14, cex=1.2)
  
  plot(c(target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1,target1$temptreat_1),c(yearwarm1[,1],yearwarm1[,2],yearwarm1[,3],yearwarm1[,4],yearwarm1[,5],yearwarm1[,6],yearwarm1[,7]),pch=rep(c(21,22,24,23), times=6),bg="black",xlab="Target warming (C)", ylab="", bty="l", xlim=c(0,6), ylim=c(0,6), cex.axis=1.2,cex.lab=1.2)
  points(c(target1$temptreat_2,target1$temptreat_2,target1$temptreat_2,target1$temptreat_2),c(yearwarm2[,1],yearwarm2[,2],yearwarm2[,3],yearwarm2[,4]),pch=rep(c(21,22,24,23), times=4), bg="black")
  points(c(target1$temptreat_3,target1$temptreat_3,target1$temptreat_3,target1$temptreat_3),c(yearwarm3[,1],yearwarm3[,2],yearwarm3[,3],yearwarm3[,4]),pch=rep(c(21,22,24,23), times=4),bg="black")
  abline(a=0,b=1,lty=1)
  legend(5,2.5,pch=c(21,22,24,23),pt.bg="black",legend=c("exp01","exp08","exp09","exp12"),bty="n")
  mtext("Target warming (C)", side=1,line=3,adj=-2.3, cex=1.2)
  
  

  @
    \section* {Supplemental Tables}
  <<label=xtable0, echo=FALSE, results=tex>>=
    library(xtable)
  library(lme4)
  library(car)
  expclim<-read.csv("expclim.csv", header=T)
  
  
  
  blockdat<-expclim[which(!is.na(expclim$block)),]
  blockdat2<-subset(blockdat,select=c(site,block,year,temptreat,soiltemp1_mean,temptreat))
  blockdat2$block<-as.factor(blockdat2$block)
  blockdat2$year<-as.factor(blockdat2$year)
  blockdat2  <- blockdat2 [apply(blockdat2 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
  blockdat2<-blockdat2[-which(blockdat2$temptreat=="ambient"),]
  testdat<-blockdat2[as.numeric(as.character(blockdat2$block))<4,]
  blockmod<-lmer(soiltemp1_mean~temptreat*block + (1|site/year), data= testdat, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
  blocktable<-round(Anova(blockmod, type="III"),digits=3)
  #blocktable2<-as.data.frame(cbind(rownames(blocktable),round(blocktable$Chisq,digits=2),blocktable$Df,round(blocktable$Pr,digits=2)))
  #colnames(blocktable2)<-c("Predictor","Chi-squared","df","p")
  #blocktable2$p[which(as.numeric(blocktable2$p)<0.05)]<-"<0.05"
  testdat2<-blockdat2[which(as.numeric(as.character(blockdat2$year))<2011),]
  testdat3<-testdat2[which(as.numeric(as.character(testdat2$year))>2008),]
  testdat3$year<-as.factor(testdat3$year)
  yearmod<-lmer(soiltemp1_mean~temptreat*year + (1|site/block), data= testdat3, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
  yeartable<-Anova(yearmod, type="III")
  #yeartable2<-as.data.frame(cbind(rownames(yeartable),round(yeartable$Chisq,digits=2),yeartable$Df,round(yeartable$Pr,digits=4)))
  #colnames(yeartable2)<-c("","Chi-squared","df","p")
  #yeartable2$p[which(as.numeric(yeartable2$p)<0.05)]<-"<0.05"
  @
    
    % EMW all tables should go into supp and give the results parenthetically in main text (sweavedemo has an example of how to do this). Also be SURE TO GIVE ERROR DF throughout. 
  
  %Yann:I know it is the direct output but don't forget for a cleaner version to just write < 0.001, write integer numbers for df and round the Chisq to keep less numbers
  <<xtable1, echo=FALSE,results=tex>>=
  require(xtable)
  xtab<-xtable(blocktable,digits=3,caption="Effects of warming vary by block, as summarized by a linear mixed effects model of mean soil temperatures, with year and site as nested random effects")
  print(xtab,floating=TRUE, include.rownames = TRUE)
  @
  
  <<xtable2, echo=FALSE,results=tex>>=
  xtab2<-xtable(yeartable,digits=3,caption="Effects of warming vary by year, as summarized by a linear mixed effects model of mean soil temperatures, with year and site as nested random effect")
  print(xtab2,floating=TRUE, include.rownames = TRUE)
  @
  \par The below are all tables related to the sham and ambient comparisons. i want to include more information in the tables, probably (random effects- intercepts, and variance), and most will be in the supplemental (perhaps just the mean soil and air in the main text?)
  .
  <<label=shamtable, echo=FALSE, results=tex>>=
  ##select out just these two treatments for now
  require(lme4)
  expclim_controls<-expclim[expclim$temptreat=="0"|expclim$temptreat=="ambient",]
  expclim_controls$temptreat<-factor(expclim_controls$temptreat)
  expclim_controls$airtemp_mean<-(expclim_controls$airtemp_min+expclim_controls$airtemp_max)/2
  sitesums<-data.frame(tapply(expclim_controls$soiltemp1_mean,list(expclim_controls$site,expclim_controls$temptreat),length))
  colnames(sitesums)<-c("sham.control","ambient")
  sites_con<-rownames(sitesums)[!is.na(sitesums$sham.control) & !is.na(sitesums$ambient)]
  expclim_cont<-expclim_controls[expclim_controls$site %in% sites_con,]
  expclim_cont$site<-as.factor(expclim_cont$site)
  colnames(expclim_cont)[2]<-"controltype"
  soilmnmod<-lmer(soiltemp1_mean~controltype + (1|site), data=expclim_cont, REML=FALSE)
  soilmimod<-lmer(soiltemp1_min~controltype + (1|site), data=expclim_cont, REML=FALSE)
  soilmxmod<-lmer(soiltemp1_max~controltype + (1|site), data=expclim_cont, REML=FALSE)
  airmnmod<-lmer(airtemp_mean~controltype + (1|site), data=expclim_cont, REML=FALSE)
  airmimod<-lmer(airtemp_min~controltype + (1|site), data=expclim_cont, REML=FALSE)
  airmxmod<-lmer(airtemp_max~controltype + (1|site), data=expclim_cont, REML=FALSE)
  airmntable<-summary(airmnmod)$coef
  airmitable<-summary(airmimod)$coef
  airmxtable<-summary(airmxmod)$coef
  soilmntable<-summary(soilmnmod)$coef
  soilmitable<-summary(soilmimod)$coef
  soilmxtable<-summary(soilmxmod)$coef
  @
  \par 
  <<shamtable1, echo=FALSE,results=tex>>=
  require(xtable)
  xtabA<-xtable(airmntable,digits=3,caption="Summary of linear mixed effects model testing difference in mean air temperatures of structural controls compared with ambient controls (i.e.with no control chambers or warming infrastructure in place). The model included a fixed effect of control type and an intercept-only random effect of studysite to account for study and measurement, as well as environmental, differences.")
  print(xtabA,floating=TRUE, include.rownames = TRUE)
  @
  
  \par
  <<shamtable1, echo=FALSE,results=tex>>=
  xtabS<-xtable(soilmntable,digits=3,caption="Summary of linear mixed effects model testing difference in mean soil temperature (at the shallowest depth measured) of structural controls compared with ambient controls. The model included a fixed effect of control type and an intercept-only random effect of studysite to account for study and measurement, as well as environmental, differences.")
  print(xtabS,floating=TRUE, include.rownames = TRUE)
  @
  
  \par
  <<shamtable1, echo=FALSE,results=tex>>=
  xtab3<-xtable(airmitable,digits=3,caption="Summary of linear mixed effects model testing difference in minimum air temperatures of structural controls compared with ambient controls (i.e.with no control chambers or warming infrastructure in place). The model included a fixed effect of control type and an intercept-only random effect of studysite to account for study and measurement, as well as environmental, differences.")
  print(xtab3,floating=TRUE, include.rownames = TRUE)
  @
  \par
  <<shamtable1, echo=FALSE,results=tex>>=
  xtab4<-xtable(soilmitable,digits=3,caption="Summary of linear mixed effects model testing difference in minimum soil temperature (at the shallowest depth measured) of structural controls compared with ambient controls. The model included a fixed effect of control type and an intercept-only random effect of studysite to account for study and measurement, as well as environmental, differences.")
  print(xtab4,floating=TRUE, include.rownames = TRUE)
  @
  
  <<shamtable1, echo=FALSE,results=tex>>=
  xtab5<-xtable(airmxtable,digits=3,caption="Summary of linear mixed effects model testing difference in maximum air temperatures of structural controls compared with ambient controls (i.e.with no control chambers or warming infrastructure in place). The model included a fixed effect of control type and an intercept-only random effect of studysite to account for study and measurement, as well as environmental, differences.")
  
  print(xtab5,floating=TRUE, include.rownames = TRUE)
  @
  
  <<shamtable1, echo=FALSE,results=tex>>=
  xtab6<-xtable(soilmxtable,digits=3,caption="Summary of linear mixed effects model testing difference in maximum soil temperature (at the shallowest depth measured) of structural controls compared with ambient controls. The model included a fixed effect of control type and an intercept-only random effect of studysite to account for study and measurement, as well as environmental, differences.")
  print(xtab6,floating=TRUE, include.rownames = TRUE)
  @
  
  
  \clearpage
  \section*{Supplemental Materials}
  \subsection*{Description of database}
  Search terms used and criteria for selecting the 12 studies that we ended up with. Climate variables included, and where database and metadata are housed.
  \subsection*{Supplemental Methods}
  \par\textit{Statistical methods}
  \par Need description of block and year analyses (see Tables 1 and 2) 
  To account for differences in the type of warming and other unmeasured site/study differences (e.g. forced air for Ellison and Marchin; heating cables for Farnsworth and ??), we fit linear mixed effects models with random effect of study-site. Response variables were daily soil or air temperature (models with daily  mean, minimum, and maximum were all fit) and , and the explanatory variable was control type (infrastructure or ambient). We used a random intercepts structure, so that the mean temperature was allowed to vary across study-sites. We fit models across the entire year, as well as separate models for each month to examine if effects varied seasonally.
  \subsection*{Random stuff moved out of the main text}
  We also found higher coefficients of variation in air temperatures in actively warmed plots, compared with controls at the same sites in the same years. This was true for both minimum and maximum air temperatures (Figure \ref{fig:cv})
  
  
  \begin{figure}[p]
  \centering
  \includegraphics{../Analyses/figures/DRAFT_CVBytreatment.pdf} 
  \caption{Ambient controls have reduced variation, compared with structural controls and nearly all warming treatments. I'm not happy with this figure- I've tried so many different ways of showing the (small but) significant differences in temperature range/variance among ambient controls, structural controls and warming treatments, in addition to the statistical analyses described in the text and supplement. Question for everyone (Lizzie/Ben/Miriam/Ann Marie/Aaron/Yann/Isabelle/Jeff/Christy): I need help with ideas for other ways to show this, or thoughts on if the point should be dropped, since the differences are minor and hard to see.} %Aaron: why not just plot variance? From this figure, the reader has to infer variance. This figure also collapses time into a single box. What about illustrating temporal variance (i.e., the time-series)? Which is what the next figure does very effectively. Yann: or maybe subtract the standard deviation of the ambient control from the structural control and finally all warming treatments?
  %Christy: I think I$B!G(Bm in favor of dropping this figure$B!D(B I think part of the problem is that there are just a ton of warming levels and your combining across a bunch of different experiments and it$B!G(Bs not clear what is different.%Jeff: I don$B!G(Bt think I understand what$B!G(Bs going on with this figure.  I am not sure what the individual data points are.  Something like Yann$B!G(Bs suggestion sounds reasonable, though.  But a more descriptive legend would really help.   Do we really want to look at some longer-term CV, or do we want something more like DTR?
  % EW: Fig 2 should be in the supplement and I had some ideas to make it look better.
  
  \label{fig:cv}
  \end{figure}





#########

<<label=xtableblocks, echo=FALSE, results=tex>>=
  library(xtable)
  require(lme4)
  require(car)
  expclim<-read.csv("expclim.csv", header=T)
  treats<-read.csv("treats_detail.csv", header=T)
  expclim2<-full_join(treats,expclim, by=c("site", "block", "plot","temptreat","preciptreat"), match="all")
  expclimt<-expclim2[which(expclim2$preciptreat==0|is.na(expclim2$preciptreat)),]#select only plots with unmanipulated precip
  expclimt$agtemp_min<-expclimt$airtemp_min
  expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$cantemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$airtemp_min) & !is.na(expclimt$cantemp_min)),]$cantemp_min
  expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$surftemp_min
  expclimt$agtemp_max<-expclimt$airtemp_max
  expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$cantemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$airtemp_max) & !is.na(expclimt$cantemp_max)),]$cantemp_max
  expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$surftemp_max
  expclimt$agtemp_mean<-(expclimt$agtemp_min+expclimt$agtemp_max)/2
  #Block and Year varation Analyses for figure showing observed vs target warming by year
  blockdat<-expclimt[which(!is.na(expclimt$block)),]#select only sites with blocked design
  #ok, try looking at differences in a few different ways:
  #First, get daily mean agtemp for each plot, find difference by block
  blockdat<-blockdat[order(blockdat$site,blockdat$block,blockdat$plot,blockdat$year,blockdat$doy),]
  blockdat2<-subset(blockdat,select=c(site,block,year,doy,temptreat,target,soiltemp1_mean,agtemp_min,agtemp_max))
  blockdat2$block<-as.factor(blockdat2$block)
  blockdat2$year<-as.factor(blockdat2$year)
  blockdat2$site<-as.factor(blockdat2$year)
  blockdat2  <- blockdat2 [apply(blockdat2 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
  #blockdat2<-blockdat2[-which(blockdat2$temptreat=="ambient"),]
  testdat<-blockdat2[as.numeric(as.character(blockdat2$block))<4,]
  blockmod<-lmer(soiltemp1_mean~temptreat*block + (1|site/year), data= testdat, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
  blockmod2<-lmer(agtemp_min~temptreat*block + (1|site/year), data= testdat, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
  blockmod3<-lmer(agtemp_max~temptreat*block + (1|site/year), data= testdat, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
  
  blocktable<-round(Anova(blockmod, type="III"),digits=3)#significant
  round(Anova(blockmod2, type="III"),digits=3)
  blocktable2<-as.data.frame(cbind(rownames(blocktable),round(blocktable$Chisq,digits=3),blocktable$Df,round(blocktable$Pr,digits=3)))
  colnames(blocktable2)<-c("Predictor","Chi-squared","df","p")
  blocktable2$p[which(as.numeric(blocktable2$p)<0.05)]<-"<0.05"
  blocktable2$Predictor<-c("(Intercept)","Temp. treatment","Block","Temp. treatment:Block")
  
  print(xtable(blocktable2,
  caption="Spatial linear mixed-effects model for soil temperature",
  label="table:blocks"),
  include.rownames=FALSE,
  caption.placement="top"
  )
  testdat2<-blockdat2[which(as.numeric(as.character(blockdat2$year))<2011),]
  testdat3<-testdat2[which(as.numeric(as.character(testdat2$year))>2008),]
  testdat3$year<-as.factor(testdat3$year)
  yearmod<-lmer(soiltemp1_mean~temptreat*year + (1|site/block), data= testdat3, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
  yeartable<-Anova(yearmod, type="III")
  yeartable2<-as.data.frame(cbind(rownames(yeartable),round(yeartable$Chisq,digits=4),yeartable$Df,round(yeartable$Pr,digits=4)))
  colnames(yeartable2)<-c("Predictor","Chi-squared","df","p")
  yeartable2$p[which(as.numeric(yeartable2$p)<0.05)]<-"<0.05"
  yeartable2$Predictor[which(yeartable2$Predictor=="temptreat")]<-"Temp. treatment"
  yeartable2$Predictor[which(yeartable2$Predictor=="temptreat:year")]<-"Temp. treatment:Block"
  yeartable2$Predictor[which(yeartable2$Predictor=="year")]<-"Year"
  
  print(xtable(yeartable2,
  caption="Temporal linear mixed-effects for mean soil temperature",
  label="table:years"),
  include.rownames=TRUE,
  caption.placement="top"
  )
  @
  
  


##Below not working for some reason:

<<label=xtable1, echo=FALSE, results=tex>>=
  library(xtable)
  require(lme4)
  require(car)
  expclim<-read.csv("expclim.csv", header=T)
  treats<-read.csv("treats_detail.csv", header=T)
  expclim2<-full_join(treats,expclim, by=c("site", "block", "plot","temptreat","preciptreat"), match="all")
  expclimt<-expclim2[which(expclim2$preciptreat==0|is.na(expclim2$preciptreat)),]#select only plots with unmanipulated precip
  expclimt$agtemp_min<-expclimt$airtemp_min
  expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$cantemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$airtemp_min) & !is.na(expclimt$cantemp_min)),]$cantemp_min
  expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$surftemp_min
  expclimt$agtemp_max<-expclimt$airtemp_max
  expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$cantemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$airtemp_max) & !is.na(expclimt$cantemp_max)),]$cantemp_max
  expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$surftemp_max
  expclimt$agtemp_mean<-(expclimt$agtemp_min+expclimt$agtemp_max)/2
  blockdat<-expclimt[which(!is.na(expclimt$block)),]#select only sites with blocked design  #First, get daily mean agtemp for each plot, find difference by block
  blockdat<-blockdat[order(blockdat$site,blockdat$block,blockdat$plot,blockdat$year,blockdat$doy),]
  blockdat2<-subset(blockdat,select=c(site,block,year,doy,temptreat,target,soiltemp1_mean,agtemp_min,agtemp_max))
  blockdat2$block<-as.factor(blockdat2$block)
  blockdat2$year<-as.factor(blockdat2$year)
  blockdat2$site<-as.factor(blockdat2$year)
  blockdat2  <- blockdat2 [apply(blockdat2 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
  testdat<-blockdat2[as.numeric(as.character(blockdat2$block))<4,]
  blockmod<-lmer(soiltemp1_mean~temptreat*block + (1|site/year), data= testdat, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
  blockmod2<-lmer(agtemp_min~temptreat*block + (1|site/year), data= testdat, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
  blockmod3<-lmer(agtemp_max~temptreat*block + (1|site/year), data= testdat, REML=FALSE,contrasts=c(unordered="contr.sum", ordered="contr.poly"))
  blocktable<-round(Anova(blockmod, type="III"),digits=3)#significant
  round(Anova(blockmod2, type="III"),digits=3)
  
  @