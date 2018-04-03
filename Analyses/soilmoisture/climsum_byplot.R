#This code does this following:
#1.Summarizes soil moisture and air temperature by plot, seasonally and annually
#2. Merge summarized climate variables in to phenology data file (expgdd) for analyses

#start by aggregating observed above-ground min and max and soil temperature by plot and year to get annual values
ag_max_plot<-aggregate(expclim2$agtemp_max, by=list(expclim2$site,expclim2$block,expclim2$plot,expclim2$target,expclim2$preciptreat_amt,expclim2$year), FUN=mean,na.rm=TRUE)
ag_min_plot<-aggregate(expclim2$agtemp_min, by=list(expclim2$site,expclim2$block,expclim2$plot,expclim2$target,expclim2$preciptreat_amt,expclim2$year), FUN=mean,na.rm=TRUE)
ag_mn_plot<-aggregate(expclim2$agtemp_mean, by=list(expclim2$site,expclim2$block,expclim2$plot,expclim2$target,expclim2$preciptreat_amt,expclim2$year), FUN=mean,na.rm=TRUE)

soilmois_plot<-aggregate(expclim2$soilmois1, by=list(expclim2$site,expclim2$block,expclim2$plot,expclim2$target,expclim2$preciptreat_amt,expclim2$year), FUN=mean,na.rm=TRUE)
#combine into one dataframe
tempsm_plots<-cbind(ag_max_plot,ag_min_plot$x,ag_mn_plot$x,soilmois_plot$x)
colnames(tempsm_plots)<-c("site","block","plot","target","preciptreat_amt","year","agtmax","agtmin","agtmean","sm")
tempsm_plots<-tempsm_plots[order(tempsm_plots$site,tempsm_plots$block,tempsm_plots$plot,tempsm_plots$year),]

#Make seasonal summaries as well: Jan-Mar, April-June
janmar<-expclim2[as.numeric(expclim2$doy)<90,]
soilmois_janmar<-aggregate(janmar$soilmois1, by=list(janmar$site,janmar$block,janmar$plot,janmar$target,janmar$preciptreat_amt,janmar$year), FUN=mean,na.rm=TRUE)
agmax_janmar<-aggregate(janmar$agtemp_max, by=list(janmar$site,janmar$block,janmar$plot,janmar$target,janmar$preciptreat_amt,janmar$year), FUN=mean,na.rm=TRUE)
agmin_janmar<-aggregate(janmar$agtemp_min, by=list(janmar$site,janmar$block,janmar$plot,janmar$target,janmar$preciptreat_amt,janmar$year), FUN=mean,na.rm=TRUE)
agmn_janmar<-aggregate(janmar$agtemp_mean, by=list(janmar$site,janmar$block,janmar$plot,janmar$target,janmar$preciptreat_amt,janmar$year), FUN=mean,na.rm=TRUE)

tempsm_janmar<-cbind(agmax_janmar,agmin_janmar$x,agmn_janmar$x,soilmois_janmar$x)

aprjun<-expclim2[as.numeric(expclim2$doy)>=90 & as.numeric(expclim2$doy)<181,]
soilmois_aprjun<-aggregate(aprjun$soilmois1, by=list(aprjun$site,aprjun$block,aprjun$plot,aprjun$target,aprjun$preciptreat_amt,aprjun$year), FUN=mean,na.rm=TRUE)
agmin_aprjun<-aggregate(aprjun$agtemp_min, by=list(aprjun$site,aprjun$block,aprjun$plot,aprjun$target,aprjun$preciptreat_amt,aprjun$year), FUN=mean,na.rm=TRUE)
agmax_aprjun<-aggregate(aprjun$agtemp_max, by=list(aprjun$site,aprjun$block,aprjun$plot,aprjun$target,aprjun$preciptreat_amt,aprjun$year), FUN=mean,na.rm=TRUE)
agmn_aprjun<-aggregate(aprjun$agtemp_mean, by=list(aprjun$site,aprjun$block,aprjun$plot,aprjun$target,aprjun$preciptreat_amt,aprjun$year), FUN=mean,na.rm=TRUE)

tempsm_aprjun<-cbind(agmax_aprjun,agmin_aprjun$x,agmn_aprjun$x,soilmois_aprjun$x)

colnames(tempsm_janmar)<-c("site","block","plot","target","preciptreat_amt","year","ag_max_janmar","ag_min_janmar","ag_mean_janmar","soilmois_janmar")

colnames(tempsm_aprjun)<-c("site","block","plot","target","preciptreat_amt","year","ag_max_aprjun","ag_min_aprjun","ag_mean_aprjun","soilmois_aprjun")

#add these to the expgdd file for later analysis

expgdd2<-left_join(expgdd,tempsm_plots,by=c("site", "block", "plot","target","preciptreat_amt","year"), copy=TRUE)
#dim(expgdd2)#63734    36
expgdd3<-left_join(expgdd2,tempsm_janmar,by=c("site", "block", "plot","target","preciptreat_amt","year"), copy=TRUE)
#dim(expgdd3)#63734    40
expgdd4<-left_join(expgdd3,tempsm_aprjun,by=c("site", "block", "plot","target","preciptreat_amt","year"), copy=TRUE)
#dim(expgdd4)#63734    44

#The following sites have both soil moisture and abg temperature data so just use these temperature data: exp01 exp02 exp03 exp04 exp07 exp09 exp10 exp12 
#check which sites have both soil moisture and abg te
ag<-tapply(expgdd4$ag_mean_aprjun,expgdd4$site,mean, na.rm=TRUE)
agsites<-names(ag[!is.na(ag)])
sm<-tapply(expgdd4$soilmois1,expgdd4$site,mean, na.rm=TRUE)
smsites<-names(sm[!is.na(sm)])
both<-smsites[which(!is.na(match(smsites,agsites)))]
#expgdd_subs<-expgdd4[!is.na(match(expgdd4$site,both)),]#not working...
expgdd_subs2<-expgdd4[which(expgdd4$site=="exp01"|expgdd4$site=="exp02"|expgdd4$site=="exp03"|expgdd4$site=="exp04"|expgdd4$site=="exp07"|expgdd4$site=="exp09"|expgdd4$site=="exp10"|expgdd4$site=="exp12"|expgdd4$site=="exp13"|expgdd4$site=="exp14"),]#
expgdd_subs2<-subset(expgdd_subs2,select=c(site,block, plot,year,styear,target,preciptreat_amt,agtmax,agtmin,agtmean,sm,doy,genus.species,event,ag_max_janmar,ag_min_janmar,ag_mean_janmar,soilmois_janmar,ag_max_aprjun,ag_min_aprjun,ag_mean_aprjun,soilmois_aprjun))
expgdd_subs <- expgdd_subs2[apply(expgdd_subs2, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

