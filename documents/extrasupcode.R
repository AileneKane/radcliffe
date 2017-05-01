
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




  
