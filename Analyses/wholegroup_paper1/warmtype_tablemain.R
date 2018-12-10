#Code to make new table comparing mean effects of OTCs with mean effects 
#of different active warming methods, by season
#code to make table s1 with basic info on study designs
#Started by Ailene February 9, 2018

rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(xtable)
require(plyr)
library(dplyr)
library(lme4)
# Set working directory: 
#if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/meta_ep2/radcliffe/documents/expwarm") 
#} else
# setwd("~Documents/radcliffe/documents/expwarm")

#Annual data from c3e
#Aggregate above-ground observed warming by block (difference between treatment and control within each block)
#to do this, i need agtemps by site, block,plot, doy, and year
expclim<-read.csv("../../Analyses/expclim.csv", header=T)
treats<-read.csv("../../Analyses/treats_detail.csv", header=T)

#want to compare mean, dtr, and variances of min and max temperatures in control plots and warmed plots in each study
expclim2<-full_join(treats,expclim, by=c("site", "block", "plot","temptreat","preciptreat"), match="first")
expclim2$styear<-NA#start by giving all studies year 1 (exp2 and exp8 had only 1 year each),then adjust each study by hand
#make a column for styear (study year, as opposed to calendar year)
sites<-unique(expclim2$site)
for (i in 1:length(sites)){
  sitedat<-expclim2[expclim2$site==sites[i],]
  styears<-unique(sitedat$year)
  #print(styears)
  for (j in 1:length(styears)){
    expclim2$styear[expclim2$site==sites[i] & expclim2$year==styears[j]]<-j
  }
}
#remove plots that manipulate precip
expclimt<-expclim2[which(expclim2$preciptreat==0|is.na(expclim2$preciptreat)),]
#get above-ground temperature
expclimt$agtemp_min<-expclimt$airtemp_min
expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$cantemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$airtemp_min) & !is.na(expclimt$cantemp_min)),]$cantemp_min
expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$surftemp_min
expclimt$agtemp_max<-expclimt$airtemp_max
expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$cantemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$airtemp_max) & !is.na(expclimt$cantemp_max)),]$cantemp_max
expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$surftemp_max
#remove site 2 (chuine) because these are not plot-level temperature measurements
expclimt<-expclimt[-which(expclimt$site=="exp02"),]
#fix mistake in control type for cedar creek site
expclimt$temptreat[expclimt$site=="exp14" & expclimt$temptreat=="0"]<-"ambient"
treats$temptreat[treats$site=="exp14" & treats$temptreat=="0"]<-"ambient"

expclimt$agtemp_mn<-(expclimt$agtemp_max+expclimt$agtemp_min)/2
expclimt$agtemp_mn[which(is.na(expclimt$agtemp_min) & is.na(expclimt$cantemp_min))]<-expclimt$airtemp_mean[which(is.na(expclimt$agtemp_min) & is.na(expclimt$cantemp_min))]
#label target temp of constrols
expclimt$target[expclimt$temptreat==0|expclimt$temptreat=="ambient"]<-0


#Add warming technique
expclimt$type<-"IR"
expclimt$type[which(expclimt$site=="exp07"|expclimt$site=="exp10"|expclimt$site=="exp15")]<-"FA"
expclimt$type[which(expclimt$site=="exp03"|expclimt$site=="exp04")]<-"FA_S"
expclimt$type[which(expclimt$site=="exp08")]<-"S"

#Add warming control type
expclimt$control<-"feedback"
expclimt$control[which(expclimt$site=="exp05"|expclimt$site=="exp06"|expclimt$site=="exp11"|expclimt$site=="exp12"|expclimt$site=="exp13"|expclimt$site=="exp14")]<-"constant"
expclimt$contype<-paste(expclimt$type,expclimt$control, sep=".")

get_mn_temp_range<-function(temptype){
  #temptype<-"agtemp_mn"
  mat<-aggregate(expclimt[,which(colnames(expclimt)==temptype)], by=list(expclimt$contype,expclimt$site,expclimt$year,expclimt$styear,expclimt$target), FUN=mean,na.rm=TRUE)
  colnames(mat)<-c("contype","site","year","styear","target","temp")
  #dim(mat)
  mat<-mat[-which(is.na(mat$temp)),]
  #dim(mat)
  #get control temps
  mat_control<-mat[mat$target==0,]
  colnames(mat_control)[6]<-"cont.temp"
  mat_control<-subset(mat_control,select=-target)
  #get difference between controls and treatments
  matdif<-join(mat, mat_control)
  matdif$tdiff<-matdif$temp-matdif$cont.temp
  matdif2<-matdif[matdif$tdiff!=0,]
  matdif2$tdiffperd<-matdif2$tdiff/matdif2$target
  
  #make table with just annual values for now- see if people like it
  mat_mean<-aggregate(matdif2$tdiff, by=list(matdif2$contype), FUN=mean,na.rm=TRUE)
  mat_min<-aggregate(matdif2$tdiff, by=list(matdif2$contype), FUN=min,na.rm=TRUE)
  mat_max<-aggregate(matdif2$tdiff, by=list(matdif2$contype), FUN=max,na.rm=TRUE)
  mat_sd<-aggregate(matdif2$tdiff, by=list(matdif2$contype), FUN=sd,na.rm=TRUE)
  mat_n<-aggregate(matdif2$tdiff, by=list(matdif2$contype), FUN=length)
  mat_se<-round(mat_sd$x/sqrt(mat_n$x), digits=2)
  colnames(mat_mean)<-c("type","mean_agt")
  mat_mean<-cbind(mat_mean,mat_se)
  mat_mean$mean_agt<-round(as.numeric(mat_mean$mean_agt), digits=2)
  mat_mean$mn_se<-paste(mat_mean$mean_agt," (", mat_mean$mat_se,")", sep="")
  mat_mean$min<-round(mat_min$x, digits=2)
  mat_mean$max<-round(mat_max$x, digits=2)
  mat_mean$range<-paste(mat_mean$min,mat_mean$max,sep="-")
  return(mat_mean)
}


get_mn_temp_perd<-function(temptype){
    #temptype<-"agtemp_mn"
    #Use block-level variation when possible, so replace NAs with 1
    expclimt$block[which(is.na(expclimt$block))]<-1
    mat<-aggregate(expclimt[,which(colnames(expclimt)==temptype)], by=list(expclimt$contype,expclimt$site,expclimt$block,expclimt$year,expclimt$styear,expclimt$target), FUN=mean,na.rm=TRUE)
    colnames(mat)<-c("contype","site","block","year","styear","target","temp")
    #dim(mat)
    mat<-mat[-which(is.na(mat$temp)),]
    #dim(mat)
    #get control temps
    mat_control<-mat[mat$target==0,]
    colnames(mat_control)[7]<-"cont.temp"
    mat_control<-subset(mat_control,select=-target)
    #get difference between controls and treatments
    matdif<-join(mat, mat_control)
    matdif$tdiff<-matdif$temp-matdif$cont.temp
    matdif2<-matdif[matdif$tdiff!=0,]
    matdif2$tdiffperd<-matdif2$tdiff/matdif2$target
    matdif2<-matdif2[-which(is.na(matdif2$temp)),]
    
    #check difference among blocks
    mat_blmean<-aggregate(matdif2$tdiff, by=list(matdif2$contype,matdif2$site,matdif2$target), FUN=mean,na.rm=TRUE)
    colnames(mat_blmean)<-c("contype","site","target","tdiff")
    
  #make table with tdiff for now- see if people like it
  mat_mean<-aggregate(matdif2$tdiffperd, by=list(matdif2$contype), FUN=mean,na.rm=TRUE)
  mat_min<-aggregate(matdif2$tdiffperd, by=list(matdif2$contype), FUN=min,na.rm=TRUE)
  mat_max<-aggregate(matdif2$tdiffperd, by=list(matdif2$contype), FUN=max,na.rm=TRUE)
  mat_sd<-aggregate(matdif2$tdiffperd, by=list(matdif2$contype), FUN=sd,na.rm=TRUE)
  mat_n<-aggregate(matdif2$tdiffperd, by=list(matdif2$contype), FUN=length)
  mat_se<-round(mat_sd$x/sqrt(mat_n$x), digits=2)
  colnames(mat_mean)<-c("type","mean_agt")
  mat_mean<-cbind(mat_mean,mat_se)
  mat_mean$mean_agt<-round(as.numeric(mat_mean$mean_agt), digits=2)
  mat_mean$mn_se<-paste(mat_mean$mean_agt," (", mat_mean$mat_se,")", sep="")
  mat_mean$min<-round(mat_min$x, digits=2)
  mat_mean$max<-round(mat_max$x, digits=2)
  mat_mean$range<-paste(mat_mean$min,mat_mean$max,sep="-")
  return(mat_mean)
}

#agt.table<-get_mn_temp_range("agtemp_mn")
#airt.table<-get_mn_temp_range("airtemp_mean")
#soilt.table<-get_mn_temp_range("soiltemp1_mean")
#expclimt$surftemp_mean<-(expclimt$surftemp_max+expclimt$surftemp_min)/2
#surft.table<-get_mn_temp_range("surftemp_mean")
#expclimt2<-expclimt[expclimt$target>0,]
#targ_mean<-aggregate(expclimt2$target, by=list(expclimt2$contype), FUN=mean,na.rm=TRUE)
#targ_min<-aggregate(expclimt2$target, by=list(expclimt2$contype), FUN=min,na.rm=TRUE)
#targ_max<-aggregate(expclimt2$target, by=list(expclimt2$contype), FUN=max,na.rm=TRUE)
agt.table<-get_mn_temp_perd("agtemp_mn")
#airt.table<-get_mn_temp_perd("airtemp_mean")
soilt.table<-get_mn_temp_perd("soiltemp1_mean")
#expclimt$surftemp_mean<-(expclimt$surftemp_max+expclimt$surftemp_min)/2
#surft.table<-get_mn_temp_perd("surftemp_mean")
expclimt2<-expclimt[expclimt$target>0,]
targ_mean<-aggregate(expclimt2$target, by=list(expclimt2$contype), FUN=mean,na.rm=TRUE)
targ_min<-aggregate(expclimt2$target, by=list(expclimt2$contype), FUN=min,na.rm=TRUE)
targ_max<-aggregate(expclimt2$target, by=list(expclimt2$contype), FUN=max,na.rm=TRUE)

targ.table<-cbind(targ_mean, round(as.numeric(targ_min$x), digits=1), round(as.numeric(targ_max$x), digits=1))
colnames(targ.table)<-c("contype","targ_mean","targ_min","targ_max")
targ.table$targ_mean<-round(as.numeric(targ.table$targ_mean), digits=1)
  
targ.table$sum<-paste(targ.table$targ_mean," (", targ.table$targ_min,"-", targ.table$targ_max,")", sep="")
contype <- data.frame(do.call('rbind', strsplit(as.character(targ.table$contype),'.',fixed=TRUE)))
#contype$site
alltypes<-cbind(c(agt.table$mn_se,""),c(agt.table$range,""), soilt.table$mn_se,soilt.table$range)
#alltypes<-rbind(otcann2,alltypes)

alltypes2<-as.data.frame(cbind(c(targ.table$sum),alltypes))
alltypes2<-cbind(contype,alltypes2)
alltypes2$n<-c("2","3","6","3","1")


alltypes2[5,3]<-5#target
alltypes2$ord<-c(5,3,1,2,4)
alltypes2<-alltypes2[order(alltypes2$ord),]
#Add in experiments?
alltypes2<-cbind(alltypes2[,1:2],c("exp05,06,11-14","exp01,02,09","exp07,10,15","exp08","exp03,04"), alltypes2[,3:9])
#add in some blank rows to accomodate the indirect effects column
alltypes3<-rbind(alltypes2[1:2,],
                 c(rep("",times=dim(alltypes2)[2])),
                 c(rep("",times=dim(alltypes2)[2])),
                 alltypes2[3,],
                 c(rep("",times=dim(alltypes2)[2])),
                 alltypes2[4,],
                 c(rep("",times=dim(alltypes2)[2])),
                 alltypes2[5,])
alltypes3$soil.drying<-c("Fig. 4, Tab. S15-16, Kimball et al. 2005",
                         "Fig.4, Tables S15-S16, ", 
                         "Kimball et al. 2005, Sherry et al. 2007",
                         "",
                        "Fig.3-4", 
                        "Norby et al. 1997",
                        "Fig.3-4, Peterjohn et al. 1993",
                        "",
                        "Fig.3-4")
alltypes3$effects<-c("+shading (Kimball et. all 2005)","+freeze-thaw cycles (McDaniel et al. 2013)", "+shading (Kimball et al. 2005)","VPD (Morin et al. 2010)","+VPD (Norby et al. 1997)", "air flow (Norby et al. 1997)","+CO2 flux (Peterjohn et al. 1993)", "+N mineralization (Peterjohn et al. 1993)","air flow (Clark et al. 2013)")

alltypes3<-alltypes3[,-10]
alltypes3[,1]<-c("infrared","infrared","","","forced air","","soil cables","","force air, soil cables")
colnames(alltypes3)<-c("warming type","warming control","study","target (min-max)","aboveground mean (se)","aboveground range","soil mean (se)","soil range","n","soil drying","other nontemperature effects")

#dim(alltypes)
#write.csv(alltypes2,"../../Analyses/output/warmtype.table.csv")

#Analyze tdif per degree of target warming in infrared plots
get_tdiff<-function(temptype){
  #temptype<-"agtemp_mn"
  mat<-aggregate(expclimt[,which(colnames(expclimt)==temptype)], by=list(expclimt$type,expclimt$site,expclimt$year,expclimt$styear,expclimt$target), FUN=mean,na.rm=TRUE)
  colnames(mat)<-c("type","site","year","styear","target","temp")
  #dim(mat)
  mat<-mat[-which(is.na(mat$temp)),]
  #dim(mat)
  #get control temps
  mat_control<-mat[mat$target==0,]
  colnames(mat_control)[6]<-"cont.temp"
  mat_control<-subset(mat_control,select=-target)
  #get difference between controls and treatments
  matdif<-join(mat, mat_control)
  matdif$tdiff<-matdif$temp-matdif$cont.temp
  matdif2<-matdif[matdif$tdiff!=0,]
  matdif2$tdiffperd<-matdif2$tdiff/matdif2$target
  return(matdif2)
}
st.tdiff<-get_tdiff("soiltemp1_mean")
st.tdiff<-st.tdiff[st.tdiff$type=="IR",]

#make table with control type by study type, so that this column can be added to the tdiff table
controltypes <- treats[treats$temptreat=="0"|treats$temptreat=="ambient",]
controltypes$site.treat<-paste(controltypes$site,controltypes$treats)
controltypes2 <- controltypes %>% # start with the data frame
  distinct(site.treat, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(site, temptreat)
colnames(controltypes2)[2]<-"control"

#controltypes2$site[controltypes2$control=="ambient"]
st.tdiff2<-left_join(st.tdiff,controltypes2, by="site")
#fit a model to see if there is a difference in tdiff by control type
#boxplot(st.tdiff2$tdiffperd~as.factor(st.tdiff2$control))
#Yikes! Big differences
test<-lmer(tdiffperd~-1+as.factor(control)+ (1|site/year), data=st.tdiff2)#add site and see what happens
#summary(test)
test.table<-round(summary(test)$coefficients[,1:2], digits=2)
rownames(test.table)<-c("structural controls","ambient controls")
colnames(test.table)<-c("Tdiff (per \\degree C of target)", "se")
#Seasonal values
#add season
#expclimt$season<-"winter"#dec22-mar21

#expclimt$season[expclimt$doy>80 & expclimt$doy<173]<-"spring"#mar22(=81)-june21 (172)
#expclimt$season[expclimt$doy>172 & expclimt$doy<264]<-"summer"#june22-sept21
#expclimt$season[expclimt$doy>263 & expclimt$doy<354]<-"fall"#sept22-dec21
#spring<-expclimt[expclimt$season=="spring",]
#winter<-expclimt[expclimt$season=="winter",]
#summer<-expclimt[expclimt$season=="summer",]
#fall<-expclimt[expclimt$season=="fall",]
#springt<-aggregate(spring$agtemp_mn, by=list(spring$type,spring$site,spring$year,spring$target), FUN=mean,na.rm=TRUE)
#summert<-aggregate(summer$agtemp_mn, by=list(summer$type,summer$site,summer$year,summer$target), FUN=mean,na.rm=TRUE)
#fallt<-aggregate(fall$agtemp_mn, by=list(fall$type,fall$site,fall$year,fall$target), FUN=mean,na.rm=TRUE)
#wintert<-aggregate(winter$agtemp_mn, by=list(winter$type,winter$site,winter$year,winter$target), FUN=mean,na.rm=TRUE)
#colnames(fallt)<-colnames(summert)<-colnames(wintert)<-colnames(springt)<-c("type","site","year","target","temp")
#fallt<-fallt[-which(is.na(fallt$temp)),]
#wintert<-wintert[-which(is.na(wintert$temp)),]
#springt<-springt[-which(is.na(springt$temp)),]
#summert<-summert[-which(is.na(summert$temp)),]
#get control temps
#fallt_control<-fallt[fallt$target==0,]
#springt_control<-springt[springt$target==0,]
#summert_control<-summert[summert$target==0,]
#wintert_control<-wintert[wintert$target==0,]

#colnames(fallt_control)[5]<-colnames(summert_control)[5]<-colnames(springt_control)[5]<-colnames(wintert_control)[5]<-"cont.temp"

#wintert_control<-subset(wintert_control,select=-target)
#fallt_control<-subset(fallt_control,select=-target)
#springt_control<-subset(springt_control,select=-target)
#summert_control<-subset(summert_control,select=-target)

#falldif<-join(fallt, fallt_control)
#falldif$tdiff<-falldif$temp-falldif$cont.temp
#springdif<-join(springt, springt_control)
#springdif$tdiff<-springdif$temp-springdif$cont.temp
#summerdif<-join(summert, summert_control)
#summerdif$tdiff<-summerdif$temp-summerdif$cont.temp
#winterdif<-join(wintert, wintert_control)
#winterdif$tdiff<-winterdif$temp-winterdif$cont.temp
#winterdif2<-winterdif[winterdif$tdiff!=0,]
#summerdif2<-summerdif[summerdif$tdiff!=0,]
#springdif2<-springdif[springdif$tdiff!=0,]
#falldif2<-falldif[falldif$tdiff!=0,]
